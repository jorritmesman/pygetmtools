#' Make a top-view plot
#'
#' Make a top-view plot (ggplot2 object), based on a data.table with 'x' and 'y' coordinates 
#'
#' @param df data.table; a data.table with columns that are specified in 'col_x', 'col_y', and 'var'.
#' @param var character; column name in 'df', to be plotted in colour
#' @param col_x,col_y character; column names of 'x' and 'y' coordinates
#' @param ... Other parameters are fed into the 'labs()' function, and 'palette' can also be specified
#' 
#' @author
#'   Jorrit Mesman
#' @examples
#'  \dontrun{
#'   df = data.table(x = c(1, 2, 3, ...),
#'                   y = c(7, 7, 7, ...),
#'                   variable = c(3.2, 1.2, 0.8, ...))
#'   plot_topview(df, "variable", col_x = "x", col_y = "y", palette = "Blues",
#'                title = "Test plot")
#'  }
#' @import ggplot2
#' @export

plot_topview = function(df, var, col_x = "x", col_y = "y", na_colour = "white", ...){
  if(!is.numeric(df[[col_x]]) | !is.numeric(df[[col_y]])){
    warning(col_x, " and/or ", col_y, " are not numeric, which will influence the plot")
  }
  
  the_palette = list(...)[["palette"]]
  if(!is.null(the_palette)){
    the_palette = "Blues"
  }
  
  ggplot(df) +
    geom_tile(aes(.data[[col_x]], .data[[col_y]], fill = .data[[var]])) +
    coord_equal(ratio = 1) + # This is a bit risky, as it may not work for some coord systems
    scale_fill_distiller(palette = the_palette, direction = 1, na.value = na_colour) +
    labs(...) +
    theme_light() +
    theme(panel.grid = element_blank())
}
