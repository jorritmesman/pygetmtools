#' Plot a transect
#'
#' Plot a transect on top of a map of the bathymetry
#'
#' @param transect data.table; a data.table with columns 'x', and 'y'.
#' @param bathy_nc character; name of the bathymetry nc file that is used as input into PyGETM
#' @param depth_name character; name of the depth variable in the bathy_nc
#' @param single_point_colour logical; if TRUE, all points are plotted in the same colour
#' @param na_colour character; NA values are set to this colour
#' @param ... Other parameters are fed into the 'labs()' function, and 'palette' can also be specified
#' 
#' @author
#'   Jorrit Mesman
#' @examples
#'  \dontrun{
#'   df = data.table(transect_id = c(1,2,3, ...),
#'                   x = c(200, 200, 200, ...),
#'                   y = c(300, 350, 400, ...))
#'   visualise_transect(transect = df, "bathymetry.nc")
#'  }
#' @import ggplot2
#' @import ncdf4
#' @export

visualise_transect = function(transect, bathy_nc, depth_name = "bathymetry",
                              single_point_colour = TRUE, na_colour = "white", ...){
  ### Add a transect id
  if(!("transect_id" %in% names(transect))){
    transect[, transect_id := 1:.N]
  }
  
  ### Read bathymetry
  nc = nc_open(bathy_nc)
  # Ensure that netcdf files are always closed, even when function crashes
  on.exit({
    nc_close(nc)
  })
  
  ### Extract depth and reformat to long
  m_depth = ncvar_get(nc, varid = depth_name)
  x_dim = ncvar_get(nc, "x")
  y_dim = ncvar_get(nc, "y")
  df_depth = data.table(m_depth)
  setnames(df_depth, as.character(y_dim))
  df_depth[, x := x_dim]
  df_depth = melt(df_depth, id.vars = "x", variable.name = "y",
                  variable.factor = F, value.name = "depth")
  df_depth[, y := as.numeric(y)]
  
  the_palette = list(...)[["palette"]]
  if(is.null(the_palette)){
    the_palette = "Greys"
  }
  
  if(single_point_colour){
    point_colour = "red"
    p = ggplot() +
      geom_tile(data = df_depth, aes(x, y, fill = depth)) +
      geom_point(data = transect, aes(x, y), colour = point_colour) +
      coord_equal(ratio = 1) + # This is a bit risky, as it may not work for some coord systems
      scale_fill_distiller(palette = the_palette, direction = 1, na.value = na_colour) +
      labs(...) +
      theme_light() +
      theme(panel.grid = element_blank())
  }else{
    point_palette = "Reds"
    p = ggplot() +
      geom_tile(data = df_depth, aes(x, y, fill = depth)) +
      geom_point(data = transect, aes(x, y, colour = transect_id)) +
      coord_equal(ratio = 1) + # This is a bit risky, as it may not work for some coord systems
      scale_fill_distiller(palette = the_palette, direction = 1, na.value = na_colour) +
      scale_colour_distiller(palette = point_palette, direction = 1, na.value = na_colour) +
      labs(...) +
      theme_light() +
      theme(panel.grid = element_blank())
  }
  return(p)
}

# Create alias for American-English spelling

#' @export
#' @rdname visualise_transect
visualize_transect = visualise_transect
