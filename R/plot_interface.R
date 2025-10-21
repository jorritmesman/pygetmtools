#' Plots interface
#'
#' @details
#'   The primary use of this function is to provide a visual confirmation of the
#'   interface that is an input in the 'flux_interface' function. Therefore it uses
#'   the same arguments. 
#' 
#' @param ncdfs  vector; vector of character name(s) of the output nc file(s). Only the
#'   first argument is used for this plotting function
#' @param interface list; list containing coordinates on the form list(direction, x, y),
#'   where direction is character ("x" or "y"), and x,y numeric, representing the
#'   x and y coordinate of the centre of the cell. Optional list arguments are "sign",
#'   which is default +1 (positive in increasing x or y) and "interface_pos", default
#'   +1 (the interface in the direction of direction*sign).
#' @param variable character; function needs a variable in order to know where there
#'   is water or not. By default 'temp'
#' @param plotting_window integer; only plot plotting_window cells around the area
#'   where you defined the interface. If Inf, plots the whole bathymetry. Default = 5L
#' @param arrow_length numeric; length of arrow "shaft", relative to width. Might need
#'   adjustment based on the size of your plot.
#' @param arrow_tip numeric; length of arrow "tip", in cm. Might need adjustment based on
#'   the size of your plot.
#' @author Jorrit Mesman
#' @examples
#'  \dontrun{
#'     plot_interface(ncdfs = "output_file.nc",
#'                    interface = list(list(x = 652650, y = 6592050, direction = "x"),
#'                                     list(x = 652650, y = 6591750, direction = "x")))
#'  }
#' @import ncdf4
#' @import ggplot2
#' @export

plot_interface = function(ncdfs, interface, variable = "temp", plotting_window = 5L,
                          arrow_length = 0.2, arrow_tip = 0.2){
  # Future potential expansions: extract profiles and variables other than discharge
  
  # Check interface arguments
  check_interface_arg(interface)
  
  nc = nc_open(ncdfs[1])
  x_coord = ncvar_get(nc, "xt")[,1]
  y_coord = ncvar_get(nc, "yt")[1,]
  var = ncvar_get(nc, variable)
  nc_close(nc)
  
  # I only need x and y and whether it is water (1) or land (NA)
  if(length(dim(var)) == 3L){
    var = var[,, dim(var)[3]]
  }else if(length(dim(var)) == 4L){
    var = var[,, dim(var)[3], dim(var)[4]]
  }
  var[!is.na(var)] = 1.0
  
  # Set boundaries of plot
  min_x = min(sapply(interface, "[[", "x"))
  max_x = max(sapply(interface, "[[", "x"))
  min_y = min(sapply(interface, "[[", "y"))
  max_y = max(sapply(interface, "[[", "y"))
  width = abs(x_coord[2] - x_coord[1])
  min_x_plot = max(min(x_coord), min_x - plotting_window * width)
  max_x_plot = min(max(x_coord), max_x + plotting_window * width)
  min_y_plot = max(min(y_coord), min_y - plotting_window * width)
  max_y_plot = min(max(y_coord), max_y + plotting_window * width)
  
  ### Create arrow location data.table, including modified x and y
  df_int = rbindlist(interface, fill = TRUE)
  if("sign" %in% names(df_int)){
    df_int[is.na(sign), sign := 1]
  }else{
    df_int[, sign := 1]
  }
  if("interface_pos" %in% names(df_int)){
    df_int[is.na(interface_pos), interface_pos := 1]
  }else{
    df_int[, interface_pos := 1]
  }
  # Add interfaces
  df_int[, `:=`(x_start = ifelse(direction == "x",
                                 x + sign(sign) * sign(interface_pos) * width / 2,
                                 x - width / 2),
                x_end = ifelse(direction == "x",
                               x + sign(sign) * sign(interface_pos) * width / 2,
                               x + width / 2),
                y_start = ifelse(direction == "y",
                                 y + sign(sign) * sign(interface_pos) * width / 2,
                                 y - width / 2),
                y_end = ifelse(direction == "y",
                               y + sign(sign) * sign(interface_pos) * width / 2,
                               y + width / 2))]
  # Add arrows
  df_int[, `:=`(x_arr_start = ifelse(direction == "x",
                                     x + sign(sign) * sign(interface_pos) * width / 2 * (1 - sign(interface_pos) * arrow_length),
                                     x),
                x_arr_end = ifelse(direction == "x",
                                   x + sign(sign) * sign(interface_pos) * width / 2 * (1 + sign(interface_pos) * arrow_length),
                                   x),
                y_arr_start = ifelse(direction == "y",
                                     y + sign(sign) * sign(interface_pos) * width / 2 * (1 - sign(interface_pos) * arrow_length),
                                     y),
                y_arr_end = ifelse(direction == "y",
                                   y + sign(sign) * sign(interface_pos) * width / 2 * (1 + sign(interface_pos) * arrow_length),
                                   y))]
  
  
  # Create plot
  df_plot = data.table(var)
  names(df_plot) = as.character(y_coord)
  df_plot[, x := x_coord]
  df_plot = melt(df_plot, id.vars = "x", variable.name = "y", variable.factor = F,
                 value.name = "depth")
  df_plot[, y := as.numeric(y)]
  p = ggplot(df_plot) +
    geom_tile(aes(x, y, fill = depth))
  # Add each line as a separate element
  p = p +
    geom_segment(data = df_int, aes(x = x_start, xend = x_end,
                                    y = y_start, yend = y_end), colour = "red")
  # Add arrows in the right direction
  p = p +
    geom_segment(data = df_int, aes(x = x_arr_start, xend = x_arr_end,
                                    y = y_arr_start, yend = y_arr_end), colour = "darkred",
                 arrow = arrow(length = unit(arrow_tip, "cm")))
  p = p +
    coord_cartesian(xlim = c(min_x_plot, max_x_plot), ylim = c(min_y_plot, max_y_plot)) +
    theme_light() +
    theme(legend.position = "none")
  
  return(p)
}
