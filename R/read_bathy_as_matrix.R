#' Read bathymetry as a matrix
#' 
#' @details
#' This function can also be used to read another variable from the bathymetry
#' ncdf file (e.g. x and y coordinates)
#'   
#' @param ncdf  character; path to a PyGETM bathymetry netcdf file
#' @param var_name  character; name of variable to extract
#' 
#' @examples
#' \dontrun{
#'    read_bathy_as_matrix("path/to/bathy.nc", var_name = "bathymetry")
#'  }
#' @import ncdf4
#' @export

read_bathy_as_matrix = function(ncdf, var_name = "bathymetry"){
  nc = nc_open(ncdf)
  var = ncvar_get(nc, varid = var_name)
  nc_close(nc)
  return(var)
}
