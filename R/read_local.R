#' Read a PyGETM local output file (1D)
#'
#' @details
#'   Read a PyGETM local output file into R, with options for extracting certain cells.
#'   Option to extract a specific z or depth, or a regular interpolated interval.
#'   pyGETM local output is created by the "transforms" argument in output.request
#'   and "pygetm.output.operators.IndexXY.parameterize"
#'
#' @param ncdf  character; name of the local output nc file
#' @param var character; name of the variable in the nc file
#' @param depth numeric; depth below surface, should be negative. If NULL, extracts all.
#'   'depth' and 'z' cannot be provided both.
#' @param z numeric; layer number (0 at bottom). If NULL, extracts all.
#'   These are the actual values, not the index (or: index in Python-counting starting at 0).
#'   'depth' and 'z' cannot be provided both.
#' @param round_depth,round_val integer; Round depth and variable value to this many digits. No rounding if NULL.
#' @param profile_interval numeric; single value, calculates output depths for a profile with
#'   this depth interval
#' @author
#'   Jorrit Mesman
#' @examples
#'  \dontrun{
#'  read_local(ncdf = "local_output.nc",
#'                     var = "temp",
#'                     depth = NULL,
#'                     z = NULL,
#'                     round_depth = 2L,
#'                     round_val = 3L)
#'  }
#' @import ncdf4
#' @export

# Note: the script assumes that multiple times are written to the local output

read_local = function(ncdf, var, depth = NULL, z = NULL, round_depth = NULL, round_val = NULL, profile_interval = NULL){
  ### Python starts at 0, so add 1 to z
  if(!is.null(z)){
    z = z + 1
  }
  
  ### Read netcdf file
  nc = nc_open(ncdf)
  # Ensure that netcdf files are always closed, even when function crashes
  on.exit({
    nc_close(nc)
  })
  
  # Extra validity check - should be a 3D PyGETM output file and 'var' should occur
  if(!(var %in% names(nc$var))){
    stop("'var' cannot be found in the 'ncdf' file!")
  }
  
  ### Extract variable
  m_all = ncvar_get(nc, varid = var)
  
  # Need to convert z into actual depths
  m_zct = ncvar_get(nc, varid = "zct") # Height of centre of cell
  if("zft" %in% names(nc$var)){
    m_zft = ncvar_get(nc, varid = "zft") # Height of cell interface
  }else{
    m_zft = array(data = NA, dim = c(dim(m_zct)[1] + 1, dim(m_zct)[2]))
    # If not saved, derive interface depths from zct (this is an estimate)
    for(t_x in seq_len(dim(m_zft)[2])){
      diff_zct = diff(m_zct[,t_x])
      diff_zct = c(diff_zct, diff_zct[length(diff_zct)])
      if(all(is.na(diff_zct)) | all(diff_zct == 0)) next
      new_zft = sapply(seq_len(length(m_zct[,t_x])), function(i) m_zct[,t_x][i] - 0.5 * diff_zct[i])
      new_zft[length(new_zft) + 1] = m_zct[,t_x][length(new_zft)] + 0.5 * diff_zct[length(new_zft) - 1]
      m_zft[,t_x] = new_zft
    }
  }
  
  m_lvl_surf = m_zft[dim(m_zft)[1],] # Height of surface
  m_lvl_bott = m_zft[1,] # Height of bottom
  rm(m_zft)
  
  df_var = slice_matrix_local(m_all, depth = depth, z = z,
                              mtrx_zct = m_zct,
                              mtrx_surf = m_lvl_surf, mtrx_bott = m_lvl_bott,
                              profile_interval = profile_interval)
  
  if(!is.null(round_depth) & "depth" %in% names(df_var)){
    df_var[, depth := round(depth, digits = round_depth)]
  }
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
  
  # Set z back to "Python-counting"
  if("z" %in% names(df_var)){
    df_var[, z := z - 1]
  }
  
  # Set correct name
  setnames(df_var, old = "val", new = var)
  
  return(df_var)
}
