#' Read x and y coordinates from a csv file into indices and return data.table
#'   
#' @param filepath  character; path to a csv file with columns "x" and "y"
#'   (or "ind_x" and "ind_y" already)
#' @param ncdf character; path to a PyGETM bathymetry netcdf file
#' 
#' @examples
#' \dontrun{
#'    read_coord_maintain_file("path/to/coord_maintain.csv", "path/to/bathy.nc")
#'  }
#' @import ncdf4
#' @export

read_coord_maintain_file = function(filepath, ncdf){
  nc = nc_open(ncdf, write = T)
  x_coord = ncvar_get(nc, varid = "x")
  y_coord = ncvar_get(nc, varid = "y")
  nc_close(nc)
  
  df = fread(filepath)
  
  # Set limits of dimensions
  max_x = length(x_coord)
  max_y = length(y_coord)
  
  # Calculate indices of df and keep indices that are inside the bounds
  if(!("ind_x" %in% names(df))){
    df[, ind_x := sapply(x, function(i) get_ind(i, x_coord, rule = "NA"))]
  }
  if(!("ind_y" %in% names(df))){
    df[, ind_y := sapply(y, function(i) get_ind(i, y_coord, rule = "NA"))]
  }
  df = df[!is.na(ind_x) & !is.na(ind_y)]
  
  return(df)
}
