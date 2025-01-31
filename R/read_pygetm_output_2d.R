#' Read a PyGETM output file (2D)
#'
#' @details
#'   Read a PyGETM 2D output file into R.
#'
#' @param ncdf  character; name of the output nc file
#' @param var character; name of the variable in the nc file
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
  if(!(var %in% names(nc$var))){
    stop("'var' cannot be found in the 'ncdf' file!")
  }
  
  ### Extract variable
  m_var = ncvar_get(nc, varid = var)
  
  # Dimensions: x, y(, time)
  x_dim = ncvar_get(nc, "xt")[, 1] # Equidistant grid, so column 1 is the same as any other
  y_dim = ncvar_get(nc, "yt")[1,]
  
  # Add time dimension if there is only one time in the file
  if(length(dim(m_var)) == 2L){
    dim(m_var) = c(dim(m_var), 1)
  }
  
  # Slice matrix
  lst_mtrx = lapply(seq_len(dim(m_var)[3]), function(t){
    tmp_df = data.table(m_var[,, t])
    setnames(tmp_df, as.character(y_dim))
    tmp_df[, `:=`(x = x_dim,
                  time_ind = t)]
    tmp_df
  })
  df_var = rbindlist(lst_mtrx)
  df_var = melt(df_var, id.vars = c("time_ind", "x"), variable.factor = F,
                variable.name = "y", value.name = "val")
  df_var[, y := as.numeric(y)]
  
  df_var = df_var[!is.na(val)]
  
  # Rounding
  if(!is.null(round_val)){
    df_var[, val := round(val, digits = round_val)]
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
  
  # Set correct name
  setnames(df_var, old = "val", new = var)
  
  return(df_var)
}
