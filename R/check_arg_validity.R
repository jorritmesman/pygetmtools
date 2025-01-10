#' Check the validity of the arguments to read_pygetm_output
#'
#' Check the validity of the input arguments to avoid difficult-to-understand errors further on
#' 
#' @param ncdf  character; name of the output nc file
#' @param x,y numeric; x and y coordinates. If NULL, extracts all.
#'   These are the actual values, not the index.
#' @param depth numeric; depth below surface, should be negative. If NULL, extracts all.
#'   These are the actual values, not the index. 'depth' and 'z' cannot be provided both.
#' @param z numeric; layer number (0 at bottom). If NULL, extracts all.
#'   These are the actual values, not the index (or: index in Python-counting starting at 0).
#'   'depth' and 'z' cannot be provided both.
#' @param transect data.table; headers 'x' and 'y', providing a set of cells to extract.
#'   Can be used in combination with depth/z or not. 'If NULL, 'x' and 'y' args are
#'   ignored of 'transect' is provided. If NULL, will not extract transect.
#' @param save_everything logical; if TRUE, exports entire output, without formatting. Defaults to FALSE

check_arg_validity = function(ncdf, x, y, depth, z, transect, save_everything){
  # Check argument types
  if((!is.character(ncdf) & !is.null(ncdf)) |
     (!is.numeric(x) & !is.null(x)) |
     (!is.numeric(y) & !is.null(y)) |
     (!is.numeric(depth) & !is.null(depth)) |
     (!is.numeric(z) & !is.null(z)) |
     (!is.data.frame(transect) & !is.null(transect)) |
     (!is.logical(save_everything) & !is.null(save_everything))){
    stop("Some argument types are wrong!")
  }
  
  if(!file.exists(ncdf)){
    stop("'ncdf' file does not exist!")
  }
  
  if(save_everything){
    # If the ncdf file and you want to save the entire matrix, then there should be no problem
    # (unless you specified the wrong variable, which will become obvious in the main function)
    return()
  }
  
  if(is.null(x) & is.null(y) & is.null(z) & is.null(depth)){
    stop("At least one of 'x', 'y', or 'z'/'depth' should be NULL, or use 'save_everything'!")
  }
  
  if(sum(!is.null(x), !is.null(y)) == 1L){
    stop("Either neither or both 'x' and 'y' need to be provided. If you want a side-view, use ",
         "the 'transect' argument!")
  }
  
  if(!is.null(z) & !is.null(depth)){
    stop("'z' and 'depth' express the same dimension and cannot be used together!")
  }
  
  if(is.null(x) & is.null(y) & is.null(z) & is.null(depth)){
    stop("At least one of 'x', 'y', 'z' or 'depth' should be specified!")
  }
  
  if(!is.null(depth)){
    if(depth > 0) stop("'depth' should be a negative number!")
  }
  
  if(!is.null(transect)){
    if(!("x" %in% names(transect) & "y" %in% names(transect))){
      stop("'transect' needs to have headers 'x' and 'y'!")
    }
  }
}
