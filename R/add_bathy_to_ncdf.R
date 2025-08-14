#' Add a matrix bathymetry to a PyGETM bathymetry netcdf file
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

add_bathy_to_ncdf = function(mtrx, ncdf, depth_name_new,
                             depth_name_orig = "bathymetry"){
  nc = nc_open(ncdf, write = T)
  
  the_vars = names(nc$var)
  
  if(depth_name_new %in% the_vars){
    nc_close(nc)
    stop("'", depth_name_new, "' already exists in ", ncdf, "!\n",
         "It must be a unique name.")
  }
  
  the_unit = nc$var[[depth_name_orig]]$units
  
  updated_bathy = ncvar_def(depth_name_new, units = the_unit,
                            dim = list(nc$dim$x, nc$dim$y),
                            missval = nc$var[[depth_name_orig]]$missval)
  ncvar_add(nc, updated_bathy, verbose = F)
  nc_close(nc)
  
  # Re-open in order to write values
  nc = nc_open(bathy_nc, write = T)
  ncvar_put(nc, varid = depth_name_new, vals = mtrx)
  nc_close(nc)
}
