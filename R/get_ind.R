#' Get index of closest value in a vector
#' 
#' Get index of closest value in a vector, and throw error if the value is out of range.
#' In case of a tie, the function returns the first index.
#' 
#' @param val  numeric; a value
#' @param vctr numeric; vector of values, should be ordered
#' @param rule character; what to do if a vector is outside the range
#' @export

get_ind = function(val, vctr, rule = "error"){
  if(rule == "error"){
    if(val < min(vctr) | val > max(vctr)){
      stop("'val' out of range of 'vctr'!")
    }
  }else if(rule == "NA"){
    if(val < min(vctr) | val > max(vctr)){
      return(as.integer(NA))
    }
  }
  # Any other argument will return the outer index
  
  which.min(abs(vctr - val))
}
