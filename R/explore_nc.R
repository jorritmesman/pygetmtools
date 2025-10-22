#' List dimensions and variables in PyGETM .nc file
#'
#' @details
#'   Show information about the PyGETM netcdf output file
#'
#' @param ncdf  character; name of the output nc file
#' @param var  character; name of a variable, default NULL (showing info about entire ncdf file if var = dim = NULL)
#' @param dim  character; name of a dimension, default NULL (showing info about entire ncdf file if var = dim = NULL). Not used if 'var' is specified.
#' 
#' @author
#'   Jorrit Mesman
#' @examples
#'  \dontrun{
#'    explore_nc("pygetm_output.nc")
#'    explore_nc("pygetm_output.nc", dim = "z")
#'    explore_nc("pygetm_output.nc", dim = "temp")
#'  }
#' @import ncdf4
#' @export

explore_nc = function(ncdf, var = NULL, dim = NULL){
  nc = nc_open(ncdf)
  
  # Ensure that netcdf files are always closed, even when function crashes
  on.exit({
    nc_close(nc)
  })
  
  if(is.null(var) & is.null(dim)){
    message("Dimensions: ", paste0(names(nc$dim), collapse = ", "))
    message("Variables: ", paste0(names(nc$var), collapse = ", "))
  }else if(!is.null(var)){
    if(!(var %in% names(nc$var))) stop(var, " not found in ", ncdf, "!")
    message("Variable longname: ", nc$var[[var]]$longname)
    message("Unit: ", nc$var[[var]]$units)
    dims = sapply(nc$var[[var]]$dim, "[[", "name")
    dim_lens = sapply(nc$var[[var]]$dim, "[[", "len")
    dim_str = sapply(seq_along(dims), function(x) paste0(dims[x], " (", dim_lens[x], ")"))
    message("Dimensions (lengths): ", paste0(dim_str, collapse = ", "))
    var_vals = ncvar_get(nc, varid = var)
    quantiles = quantile(var_vals, na.rm = T)
    q_names = names(quantiles)
    q_str = sapply(seq_along(q_names), function(x) paste0(q_names[x], " - ", quantiles[x]))
    message("Quantiles: ", paste0(q_str, collapse = ", "))
  }else if(!is.null(dim)){
    if(!(dim %in% names(nc$dim))) stop(dim, " not found in ", ncdf, "!")
    message("Dimension name: ", dim)
    message("Length: ", nc$dim[[dim]]$len)
    quantiles = quantile(nc$dim[[dim]]$vals)
    q_names = names(quantiles)
    q_str = sapply(seq_along(q_names), function(x) paste0(q_names[x], " - ", quantiles[x]))
    message("Quantiles: ", paste0(q_str, collapse = ", "))
  }
}
