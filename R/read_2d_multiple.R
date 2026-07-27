#' Read multiple PyGETM 2D output files and merge them
#'
#' @details
#'   Repeated calls to 'read_pygetm_output_2d' and merging them into a single
#'   data.table. A common way of running PyGETM is to save outputs from different
#'   months into separate netcdf files, and this function would merge them into
#'   one data.table.
#'
#' @param ncdfs  character; vector with the names of the output nc files
#' @param var character; name of the variable in the nc file. Can be a vector.
#' @param round_val integer; Round depth and variable value to this many digits. No rounding if NULL.
#' @author
#'   Jorrit Mesman
#' @examples
#'  \dontrun{
#'  read_2d_multiple(ncdfs = c("output2d_20210101.nc", "output2d_20210201.nc"),
#'                     var = "zt",
#'                     round_val = 5L)
#'  }
#' @export

read_2d_multiple = function(ncdfs, var, round_val = NULL){
  pb = txtProgressBar(min = 0, max = length(ncdfs), style = 3)
  
  lst_files = lapply(seq_along(ncdfs), function(ind){
    setTxtProgressBar(pb, ind)
    read_pygetm_output_2d(ncdf = ncdfs[ind], var = var, round_val = round_val)
  })
  
  df_all = rbindlist(lst_files)
  setorder(df_all, date, x, y)
  
  return(df_all)
}
