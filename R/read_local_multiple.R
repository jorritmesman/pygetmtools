#' Read multiple local PyGETM output files and merge them
#'
#' @details
#'   Repeated calls to 'read_local' and merging them into a single
#'   data.table. A common way of running PyGETM is to save outputs from different
#'   months into separate netcdf files, and this function would merge them into
#'   one data.table.
#'   
#'   There are Several options for extracting output. 
#'
#' @param ncdfs  character; vector with the names of the output nc files
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
#'  read_multiple(ncdfs = c("output_local_20210101.nc", "output_local_20210201.nc"),
#'                     var = "temp",
#'                     depth = NULL,
#'                     z = 19,
#'                     round_depth = 2L,
#'                     round_val = 3L)
#'  }
#' @export

read_local_multiple = function(ncdfs, var, depth = NULL, z = NULL, round_depth = NULL,
                               round_val = NULL, profile_interval = NULL){
  pb = txtProgressBar(min = 0, max = length(ncdfs), style = 3)
  
  lst_files = lapply(seq_along(ncdfs), function(ind){
    setTxtProgressBar(pb, ind)
    read_local(ncdf = ncdfs[ind], var = var, depth = depth, z = z, round_depth = round_depth,
               round_val = round_val, profile_interval = profile_interval)
  })
  
  df_all = rbindlist(lst_files)
  setorder(df_all, date)
  
  return(df_all)
}
