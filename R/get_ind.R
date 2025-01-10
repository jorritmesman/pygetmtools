#' Get index of closest value in a vector
#' 
#' Get index of closest value in a vector, and throw error if the value is out of range.
#' In case of a tie, the function returns the first index.
#' 
#' @param val  numeric; a value
#' @param vctr numeric; vector of values, should be ordered

get_ind = function(val, vctr){
  if(val < min(vctr) | val > max(vctr)){
    stop("'val' out of range of 'vctr'!")
  }
  which.min(abs(vctr - val))
}
