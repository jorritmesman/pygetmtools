#' Plot an interactive bathymetry plot
#'
#' @details
#'   An interactive bathymetry plot to display coordinates and depth of a cell by hovering over the plot.
#'   By clicking, a pop-up message returns the coordinates and the depth. 
#'   The coordinates can be geographic ('lon', 'lat' in degrees), projected ('x', 'y' in meters), or grid indices ('i', 'j').
#'
#' @param ncdf Character. Path to the NetCDF bathymetry file.
#' @param coord_type Character. One of 'projected', 'geographic', or 'indices'. The type of coordinates to display in the plot.
#' @author
#'   Muhammed Shikhani
#' @examples
#'  \dontrun{
#'    plot_interactive_bathymetry("Bathymetry/bathymetry.nc", coord_type = "projected")
#'  }
#' @import ncdf4
#' @import ggplot2
#' @import plotly
#' @import viridis
#' @import htmlwidgets
#' @export

plot_interactive_bathymetry = function(ncdf, bathy_name = "bathymetry", coord_type = c("indices", "projected", "geographic")){
  
  nc_file = nc_open(ncdf)
  bath = ncvar_get(nc_file, bathy_name)
  
  coord_names = c(names(nc_file$var), names(nc_file$dim))
  
  x_name   = get_dim_name(c("x", "X"), coord_names)
  y_name   = get_dim_name(c("y", "Y"), coord_names)
  lon_name = get_dim_name(c("lon", "longitude", "Longitude"), coord_names)
  lat_name = get_dim_name(c("lat", "latitude",  "Latitude"), coord_names)
  
  if(length(coord_type) > 1L){
    coord_type = coord_type[1]
    message("Multiple arguments provided for 'coord_type'; using only first one.")
  }
  
  if(coord_type == "projected" | coord_type == "indices"){
    x_coord = ncvar_get(nc_file,  x_name)
    y_coord = ncvar_get(nc_file,  y_name)
    
    if(coord_type == "indices"){
      x_coord = order(x_coord)
      y_coord = order(y_coord)
      coord_labels = c("X Index", "Y Index")
      plot_title = "Bathymetry Plot (Indices)"
    }else{
      coord_labels = c("X Coordinate", "Y Coordinate")
      plot_title = "Bathymetry Plot (Projected Coordinates)"
    }
    coord_names = c("x", "y")
  }else if (coord_type == "geographic"){
    lon_coord = ncvar_get(nc_file, lon_name)
    lat_coord = ncvar_get(nc_file, lat_name)
    lon_coord =lon_coord[,as.integer(ncol(lon_coord)/2)]
    lat_coord =lat_coord[as.integer(nrow(lat_coord)/2),]
    coord_names = c("lon", "lat")
    coord_labels = c("Longitude", "Latitude")
    plot_title = "Bathymetry Plot (Geographic Coordinates)"
  }else{
    stop("Invalid argument for 'coord_type'!")
  }
  
  # Melt (base-R style)
  bath_df = data.frame(
    expand.grid(lapply(dim(bath), seq_len)),
    value = as.vector(bath)
  )
  names(bath_df) = c(coord_names, "depth")
  
  # # Original code (caused conflict with dt melt)
  # bath_df2 = reshape2::melt(bath, varnames = coord_names, value.name = "depth")
  
  if (coord_type == "projected" | coord_type == "indices") {
    bath_df$x = x_coord[bath_df$x]
    bath_df$y = y_coord[bath_df$y]
  } else if (coord_type == "geographic") {
    bath_df$lon = lon_coord[bath_df$lon]
    bath_df$lat = lat_coord[bath_df$lat]
  }
  
  bath_df$hover_text = paste0(
    coord_labels[1], ": ", round(bath_df[[coord_names[1]]], 5), "<br>",
    coord_labels[2], ": ", round(bath_df[[coord_names[2]]], 5), "<br>",
    "Depth: ", round(bath_df$depth, 5)
  )
  
  p = ggplot(bath_df, aes(
    x = .data[[coord_names[1]]],
    y = .data[[coord_names[2]]],
    fill = .data[["depth"]],
    text = .data[["hover_text"]]
  )) +
    geom_tile() +
    scale_fill_viridis(option = "C") +
    theme_minimal() +
    labs(
      title = plot_title,
      x = coord_labels[1],
      y = coord_labels[2],
      fill = "Depth"
    )
  
  interactive_plot = ggplotly(p, tooltip = "text")
  
  interactive_plot = onRender(interactive_plot, "
    function(el, x) {
      el.on('plotly_click', function(data) {
        var pt = data.points[0];
        var msg = pt.text.replace(/<br>/g, '\\n');
        alert('Clicked Point:\\n' + msg);
      });
    }
  ")
  
  nc_close(nc_file)
  
  return(interactive_plot)
}
