#' Make a side-view plot
#'
#' @details
#'   Make a side-view plot (ggplot2 object), based on a data.table with 'd' and 'depth' columns.
#'   If 'depth' is not there, the function will search for a 'z' column and plot that instead.
#'
#' @param df data.table; a data.table with columns that are specified in 'col_id' and 'col_depth'.
#' @param var character; column name in 'df', to be plotted in colour
#' @param col_id,col_depth character; column names of 'id' and 'depth' values
#' @param surface_lvl numeric; single value or vector with the level of the surface above the sediment
#' @param na_colour character; colour of NA values
#' @param ... Other parameters are fed into the 'labs()' function, and 'palette' can also be specified
#' 
#' @author
#'   Jorrit Mesman
#' @examples
#'  \dontrun{
#'    df = data.table(transect_id = c(1, 1, 1, 2, 2, 2, ...),
#'                    depth = c(-0.5, -1.5, -2.5, -0.5, -1.5, -2.5, ...),
#'                    variable = c(3.2, 1.2, 0.8, 3.6, 1.9, 0.5, ...))
#'    plot_topview(df, "variable", col_x = "x", col_y = "y", palette = "Blues",
#'                title = "Test plot")
#'  }
#' @import ggplot2
#' @export

plot_sideview = function(df, var, col_id = "transect_id", col_depth = "depth", surface_lvl = NULL, na_colour = "white", ...){
  df = copy(df)
  
  if(!(col_id %in% names(df))) stop(col_id, " must be one of the columns in 'df'!")
  
  if(!(col_depth %in% names(df))){
    stop("No valied depth-column provided, nor a valid z-column!")
  }
  
  if(!is.null(surface_lvl)){
    warning("'surface_lvl' argument not implemented yet; plots are relative to the surface level.")
  }
  
  the_palette = list(...)[["palette"]]
  if(is.null(the_palette)){
    the_palette = "Blues"
  }
  
  is_regularly_spaced = check_regular_spacing(df, col_date = "date", col_id = col_id, col_depth = col_depth)
  if(!is_regularly_spaced){
    warning("It seems that the depths in your data.table are not regularly spaced. 'plot_sideview' was designed ",
            "to work with regularly spaced depths and the plot may not look as expected. Consider using the",
            "'profile_interval' option in the 'read_' functions.")
  }
  
  ggplot(df) +
    geom_tile(aes(.data[[col_id]], .data[[col_depth]], fill = .data[[var]])) +
    scale_fill_distiller(palette = the_palette, direction = 1, na.value = na_colour) +
    labs(...) +
    theme_light() +
    theme(panel.grid = element_blank())
}
