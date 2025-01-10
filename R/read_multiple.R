#' Read multiple PyGETM 3D output files and merge them
#'
#' @details
#'   Repeated calls to 'read_pygetm_output' and merging them into a single
#'   data.table. A common way of running PyGETM is to save outputs from different
#'   months into separate netcdf files, and this function would merge them into
#'   one data.table.
#'   
#'   There are Several options for extracting output. Either everything
#'   ('save_everything' is TRUE), 2D from the top ('x' and 'y' are NULL and
#'   specifying 'depth' or 'z'), a 1D depth profile (specifying 'x' and 'y'
#'   and both 'depth' and 'z' are NULL), a single point ('x', 'y', and 'depth/z'),
#'   or 1D/2D from the "side" (provided 'transect' and optionally 'depth/z').
#'
#' @param ncdfs  character; vector with the names of the output nc files
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
#'  read_multiple(ncdfs = c("output_20210101.nc", "output_20210201.nc"),
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

read_multiple = function(ncdfs, var, x = NULL, y = NULL, depth = NULL, z = NULL, transect = NULL, save_everything = F,
                         round_depth = NULL, round_val = NULL){
  pb = txtProgressBar(min = 0, max = length(ncdfs), style = 3)
  
  lst_files = lapply(seq_along(ncdfs), function(ind){
    setTxtProgressBar(pb, ind)
    read_pygetm_output(ncdf = ncdfs[ind], var = var, x = x, y = y, depth = depth,
                       z = z, transect = transect, save_everything = save_everything,
                       round_depth = round_depth, round_val = round_val)
  })
  
  df_all = rbindlist(lst_files)
  setorder(df_all, date, x, y)
  
  return(df_all)
}
