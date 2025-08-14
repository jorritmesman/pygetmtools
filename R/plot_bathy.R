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
#'   There is another version of this function - 'plot_interactive_bathymetry' - where
#'   you can check individual cell values in the RStudio viewer. That only works when
#'   providing a netcdf file
#'   
#' @param mtrx  matrix; numeric matrix, describing a bathymetry
#' @param mtrx_ref  matrix; numeric matrix, describing a bathymetry. If provided,
#'   plot will show the difference between mtrx - mtrx_ref. Default is NULL.
#' @param rev_x,rev_y logical; if TRUE, will plot that axis in reverse
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

plot_bathy = function(mtrx, mtrx_ref = NULL, rev_x = FALSE, rev_y = FALSE){
  if(is.null(mtrx_ref)){
    df_plot = data.table(mtrx)
  }else{
    if(!identical(dim(mtrx), dim(mtrx_ref))){
      stop("'mtrx' and 'mtrx_ref' do not have the same dimensions!")
    }
    df_plot = data.table(mtrx - mtrx_ref)
  }
  names(df_plot) = as.character(seq_len(ncol(df_plot)))
  df_plot[, x := seq_len(nrow(df_plot))]
  
  df_plot = melt(df_plot, id.vars = "x", variable.name = "y", variable.factor = F,
                 value.name = "depth")
  df_plot[, y := as.numeric(y)]
  p = ggplot(df_plot) +
    geom_tile(aes(x, y, fill = depth))
  if(rev_x) p = p + scale_x_reverse()
  if(rev_y) p = p + scale_y_reverse()
  return(p)
}
