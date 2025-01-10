#' Extract a value for specific depths from a profile
#'
#' @details
#'  A function to extract a value from a certain depth ("the_depth")
#   Depths are assumed to be negative, relative to the water surface
#   'depths' refers to the centre of the pygetm cells ('zct')
#   Values between depths are calculated from linear interpolation, and
#   depths above or below the upper and lower cell, respectively, are set
#   to that cell's value. Positive 'the_depth' generates an error and a
#   'the_depth' lower than the bottom generates NA.
#'
#' @param vals numeric; vector of values, same length as depths
#' @param depths numeric; ordered vector of values, same length as depths
#' @param the_depth numeric; depth for which to extract the value.
#' @param depth_bott numeric; depth below surface of the bottom (negative).
#' @author
#'   Jorrit Mesman
#' @examples
#'  calc_depth_from_z(vals = c(3.5, 4, 6.8, 7.1, 8.9),
#'                    depths = c(-4.6, -3.9, -2.8, -1.9, -0.7),
#'                    the_depth = -3,
#'                    depth_bott = -5.2)
#' @export

extract_from_profile = function(vals, depths, the_depth, depth_bott){
  if(the_depth > 0) stop("'the_depth' cannot be positive!")
  if(the_depth < depth_bott){
    return(as.numeric(NA))
  }
  
  approx(depths, y = vals, xout = the_depth)$y
}
