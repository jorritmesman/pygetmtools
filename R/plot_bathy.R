#' Plot a bathymetry
#'
#' @details
#'   Creates a ggplot2 object that can be modified further.
#'   
#'   Takes a matrix. Can be gotten from a netcdf file by 'read_bathy_as_matrix'
#'   
#'   Either plots the bathymetry or, if 'bathy_ref' is provided, the difference
#'   between the two bathymetries.
#'   
#'   If a data.table with fixed coordinates is provided, this is plotted on top
#'   of the map
#'   
#'   There is another version of this function - 'plot_interactive_bathymetry' - where
#'   you can check individual cell values in the RStudio viewer. That only works when
#'   providing a netcdf file
#'   
#' @param mtrx  matrix; numeric matrix, describing a bathymetry
#' @param mtrx_ref  matrix; numeric matrix, describing a bathymetry. If provided,
#'   plot will show the difference between mtrx - mtrx_ref. Default is NULL.
#' @param plot_slope_method character; "diff" or "rx0"; makes a map of the slope
#'   parameter instead. Will ignore the 'mtrx_ref' argument. Default is NULL.
#' @param rev_x,rev_y logical; if TRUE, will plot that axis in reverse
#' @param maintain_coords data.table; has columns 'ind_x' and 'ind_y'
#'   
#' @author
#'   Jorrit Mesman
#' @examples
#' mat = matrix(c(NA, 3.1, 1.1, 10.1, NA,
#'                NA, NA, 14.5, 19.2, NA,
#'                NA, 1.0, 7.7, 22.0, 16.5,
#'                4.2, 3.5, 6.9, 13.0, 12.7,
#'                NA, NA, 5.1, NA, NA),
#'              nrow = 5L, ncol = 5L)
#' plot_bathy(mat)
#'  
#' @import ggplot2
#' @export

plot_bathy = function(mtrx, mtrx_ref = NULL,
                      plot_slope_method = NULL, rev_x = FALSE, rev_y = FALSE,
                      maintain_coords = NULL){
  if(!is.null(plot_slope_method)){
    if(is.logical(plot_slope_method)){
      stop("'plot_slope_method' should be 'diff' or 'rx0'!")
    }else if(!(plot_slope_method %in% c("diff", "rx0"))){
      stop("'plot_slope_method' should be 'diff' or 'rx0'!")
    }
    mtrx_plot = mtrx
    
    for(i in seq_len(dim(mtrx_plot)[1])){
      for(j in seq_len(dim(mtrx_plot)[2])){
        if(plot_slope_method == "diff"){
          mtrx_plot[i, j] = calc_max_diff(mtrx, i, j)
        }else if(plot_slope_method == "rx0"){
          mtrx_plot[i, j] = calc_max_rx0(mtrx, i, j)
        }
      }
    }
    df_plot = data.table(mtrx_plot)
    colour_name = plot_slope_method
  }else if(is.null(mtrx_ref)){
    df_plot = data.table(mtrx)
    colour_name = "depth"
  }else{
    if(!identical(dim(mtrx), dim(mtrx_ref))){
      stop("'mtrx' and 'mtrx_ref' do not have the same dimensions!")
    }
    df_plot = data.table(mtrx - mtrx_ref)
    colour_name = "depth diff."
  }
  
  names(df_plot) = as.character(seq_len(ncol(df_plot)))
  df_plot[, x := seq_len(nrow(df_plot))]
  
  df_plot = melt(df_plot, id.vars = "x", variable.name = "y", variable.factor = F,
                 value.name = "depth")
  df_plot[, y := as.numeric(y)]
  p = ggplot(df_plot) +
    geom_tile(aes(x, y, fill = depth))
  if(!is.null(maintain_coords)){
    p = p +
      geom_point(data = maintain_coords,aes(x = ind_x, y = ind_y),
                 colour = "red")
  }
  if(rev_x) p = p + scale_x_reverse()
  if(rev_y) p = p + scale_y_reverse()
  if(!is.null(plot_slope_method) | is.null(mtrx_ref)){
    p = p + scale_fill_gradient(low = "white", high = "blue",
                                name = colour_name)
  }else{
    p = p + scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.0,
                                 name = colour_name)
  }
  p = p + theme_light()
  
  return(p)
}
