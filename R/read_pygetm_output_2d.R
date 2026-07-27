#' Read a PyGETM output file (2D)
#'
#' @details
#'   Read a PyGETM 2D output file into R.
#'
#' @param ncdf  character; name of the output nc file
#' @param var character; name of the variable in the nc file. Can be a vector.
#' @param round_val integer; Round depth and variable value to this many digits. No rounding if NULL.
#' @author
#'   Jorrit Mesman
#' @examples
#'  \dontrun{
#'   read_pygetm_output_2d(ncdf = "pygetm_output_2d.nc",
#'                         var = "zt",
#'                         round_val = 3L)
#'  }
#' @import ncdf4
#' @export

read_pygetm_output_2d = function(ncdf, var, round_val = NULL){
  ### Read netcdf file
  nc = nc_open(ncdf)
  # Ensure that netcdf files are always closed, even when function crashes
  on.exit({
    nc_close(nc)
  })
  
  # 'var' should occur in the file
  if(any(var %notin% names(nc$var))){
    stop("'", paste(var[var %notin% names(nc$var)], collapse = ", "),
         "' cannot be found in the 'ncdf' file!")
  }
  
  ### Extract variable
  m_var = lapply(var, function(x) ncvar_get(nc, varid = x))
  names(m_var) = var
  
  # Dimensions: x, y(, time)
  x_dim = ncvar_get(nc, "xt")[, 1] # Equidistant grid, so column 1 is the same as any other
  y_dim = ncvar_get(nc, "yt")[1,]
  
  # Add time dimension if there is only one time in the file
  if(length(dim(m_var[[1]])) == 2L){
    for(i in seq_len(length(m_var))){
      dim(m_var[[i]]) = c(dim(m_var[[i]]), 1)
    }
  }
  
  # Slice matrix
  lst_tmp = lapply(names(m_var), function(var_nm){
    rbindlist(
      lapply(seq_len(dim(m_var[[var_nm]])[3]), function(t){
        tmp_df = data.table(m_var[[var_nm]][, , t])
        setnames(tmp_df, as.character(y_dim))
        tmp_df[, `:=`(x = x_dim,
                      time_ind = t)]
        tmp_df = melt(tmp_df,
                      id.vars = c("time_ind", "x"),
                      variable.factor = FALSE,
                      variable.name = "y",
                      value.name = var_nm)
        tmp_df[, y := as.numeric(y)]
        tmp_df
      })
    )
  })
  
  df_var = Reduce(
    function(x, y){
      merge(x, y, by = c("time_ind", "x", "y"), all = TRUE)
    },
    lst_tmp
  )
  
  keep = if(length(var) == 1){
    !is.na(df_var[[var]])
  }else{
    df_var[, rowSums(!is.na(.SD)) > 0, .SDcols = var]
  }
  
  df_var = df_var[keep]
  if(nrow(df_var) == 0L){
    message("No data on this location.")
    return(df_var)
  }
  
  # Rounding
  if(!is.null(round_val)){
    for(i in seq_along(var)){
      the_name = var[i]
      if(length(round_val) == 1L){
        df_var[, (the_name) := round(get(the_name), digits = round_val)]
      }else{
        df_var[, (the_name) := round(get(the_name), digits = round_val[i])]
      }
    }
  }
  
  # Convert time_ind to an actual date
  tim = ncvar_get(nc, "time")
  tunits = ncatt_get(nc, "time")
  tustr = strsplit(tunits$units, " ")
  step = tustr[[1]][1]
  origin = as.POSIXct(paste(tustr[[1]][3], tustr[[1]][4]),
                      format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  multiplier = fcase(step == "days", 24 * 60 * 60,
              step == "hours", 60 * 60,
              step == "minutes", 60,
              step == "seconds", 1)
  dict_time = as.list(tim * multiplier)
  names(dict_time) = seq_len(length(dict_time))
  df_var[, time_ind := dict_convert(time_ind, dict_time)]
  df_var[, time_ind := as.POSIXct(as.numeric(time_ind), origin = origin, tz = "UTC")]
  setnames(df_var, old = "time_ind", new = "date")
  
  return(df_var)
}
