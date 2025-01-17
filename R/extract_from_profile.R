#' Extract values for specific depths from a profile
#'
#' @details
#'  A function to extract (a) value(s) from specific depths ("depths_out")
#'  Depths are assumed to be negative, relative to the water surface
#'  'depths' refers to the centre of the pygetm cells ('zct')
#'  Values between depths are calculated from linear interpolation, and
#'  depths above or below the upper and lower cell, respectively, are set
#'  to that cell's value. Positive 'the_depth' generates an error and 
#'  'depths_out' lower than the bottom return NA.
#'
#' @param vals numeric; vector of values, same length as depths
#' @param depths numeric; ordered vector of values, same length as depths
#' @param depths_out numeric; vector of depths for which to extract the value.
#' @param depth_bott numeric; depth below surface of the bottom (negative).
#' @author
#'   Jorrit Mesman
#' @examples
#'  extract_from_profile(vals = c(3.5, 4, 6.8, 7.1, 8.9),
#'                       depths = c(-4.6, -3.9, -2.8, -1.9, -0.7),
#'                       depths_out = seq(-6, -0.5, by = 0.5),
#'                       depth_bott = -5.2)
#' @export

extract_from_profile = function(vals, depths, depths_out, depth_bott){
  if(any(depths_out > 0)) stop("None of 'depths_out' should be positive!")
  
  vals_out = approx(depths, y = vals, xout = depths_out, rule = 2)$y
  
  # Below depth_bott, set values to NA 
  vals_out[depths_out < depth_bott] = NA
  
  return(vals_out)
}
