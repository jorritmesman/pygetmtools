#' Standardise coordinates
#'
#' @param options Character. Potential names to check
#' @param coord_names Character. Vector of provided values in netcdf file
#' @author
#'   Muhammed Shikhani

get_dim_name = function(options, coord_names){
  match = options[options %in% coord_names]
  if (length(match) > 0) {
    return(match[1])
  } else {
    stop("No matching dimension name found")
  }
}
