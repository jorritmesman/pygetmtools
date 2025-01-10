#' Read a PyGETM output file (3D)
#'
#' @details
#'   Read a PyGETM 3D output file into R, with options for extracting certain cells.
#'   Several options for extracting output. Either everything ('save_everything' is TRUE),
#'   2D from top ('x' and 'y' are NULL and specifying 'depth' or 'z'), a 1D depth profile
#'   (specifying 'x' and 'y' and both 'depth' and 'z' are NULL), a single point ('x', 'y',
#'   and 'depth/z'), or 1D/2D from the "side" (provided 'transect' and optionally 'depth/z').
#'
#' @param ncdf  character; name of the output nc file
#' @param var character; name of the variable in the nc file
#' @param x,y numeric; x and y coordinates. If NULL, extracts all.
#' @param depth numeric; depth below surface, should be negative. If NULL, extracts all.
#'   'depth' and 'z' cannot be provided both.
#' @param z numeric; layer number (0 at bottom). If NULL, extracts all.
#'   These are the actual values, not the index (or: index in Python-counting starting at 0).
#'   'depth' and 'z' cannot be provided both.
#' @param transect data.table; headers 'x' and 'y', providing a set of cells to extract.
#'   Can be used in combination with depth/z or not. 'If NULL, 'x' and 'y' args are
#'   ignored of 'transect' is provided. If NULL, will not extract transect.
#' @param save_everything logical; if TRUE, exports entire output, without formatting. Defaults to FALSE
#' @param round_depth,round_val integer; Round depth and variable value to this many digits. No rounding if NULL.
#' @author
#'   Jorrit Mesman
#' @examples
#'  \dontrun{
#'  read_pygetm_output(ncdf = "pygetm_output_3d.nc",
#'                     var = "temp",
#'                     x = NULL,
#'                     y = NULL,
#'                     depth = NULL,
#'                     z = 19,
#'                     transect = NULL,
#'                     save_everything = FALSE,
#'                     round_depth = 2L,
#'                     round_val = 3L)
#'  }
#' @export

read_pygetm_output = function(ncdf, var, x = NULL, y = NULL, depth = NULL, z = NULL, transect = NULL, save_everything = F,
                              round_depth = NULL, round_val = NULL){
  ### Check validity of input arguments
  check_arg_validity(ncdf = ncdf, x = x, y = y, depth = depth, z = z, transect = transect, save_everything = save_everything)
  
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
  
  # Dimensions: x, y, z, time
  x_dim = ncvar_get(nc, "xt")[, 1] # Equidistant grid, so column 1 is the same as any other
  y_dim = ncvar_get(nc, "yt")[1,]
  
  ### Slice and format the matrix into the desired output
  if(save_everything){
    nc_close(nc)
    return(m_all)
  }
  
  # Need to convert z into actual depths
  m_zct = ncvar_get(nc, varid = "zct") # Height of centre of cell
  m_zft = ncvar_get(nc, varid = "zft") # Height of cell interface
  # Add time dimension if there is only one time in the file
  if(length(dim(m_all)) == 3L){
    dim(m_all) = c(dim(m_all), 1)
    dim(m_zct) = c(dim(m_zct), 1)
    dim(m_zft) = c(dim(m_zft), 1)
  }
  m_lvl_surf = m_zft[,, dim(m_all)[3] + 1,] # Height of surface
  m_lvl_bott = m_zft[,, 1,] # Height of bottom
  rm(m_zft)
  
  df_var = slice_matrix(m_all, x_dim = x_dim, y_dim = y_dim,
                        x = x, y = y, depth = depth, z = z,
                        transect = transect, mtrx_zct = m_zct,
                        mtrx_surf = m_lvl_surf, mtrx_bott = m_lvl_bott)
  
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
