#' Check the validity of the 'interface' argument
#'
#' Check the validity of the 'interface' argument to the flux_interface and plot_interface functions
#' 
#' @param interface list; list containing coordinates on the form list(direction, x, y),
#'   where direction is character ("x" or "y"), and x,y numeric, representing the
#'   x and y coordinate of the centre of the cell. Optional list arguments are "sign",
#'   which is default +1 (positive in increasing x or y) and "interface_pos", default
#'   +1 (the interface in the direction of direction*sign). Can be visualised with
#'   function 'plot_interface'.

check_interface_arg = function(interface){
  known_args = c("x", "y", "direction", "sign", "interface_pos")
  must_args = c("x", "y", "direction")
  sapply(interface, function(x) if(any(names(x) %notin% known_args)){
    stop("Unknown arguments found in 'interface' argument: ",
         paste0(names(x)[names(x) %notin% known_args], collapse = ", "))
  })
  sapply(interface, function(x) if(any(must_args %notin% names(x))){
    stop("All these arguments must be present in each 'interface' cell: ",
         must_args)
  })
  all_dirs = unique(sapply(interface, function(x) x[["direction"]]))
  if(any(all_dirs %notin% c("x", "y"))) stop("'direction' argument of ",
                                             "'interface' must be 'x' or 'y'")
  
  current_coord = 1L
  for(coordinate in interface){
    # Check that all coordinates are formatted correctly
    if((is.null(coordinate$x) | !is.numeric(coordinate$x)) |
       (is.null(coordinate$y)| !is.numeric(coordinate$y)) |
       (is.null(coordinate$direction) | !is.character(coordinate$direction))){
      stop("The coordinate ", current_coord, "has wrong or missing input. Should be on the form list(direction = character, x = numeric, y = numeric)")
    }
    
    if(!is.null(coordinate$sign)){
      if(!is.numeric(coordinate$sign)) stop("The coordinate ", current_coord, ": the 'sign' argument should be numeric!")
    }
    if(!is.null(coordinate$interface_pos)){
      if(!is.numeric(coordinate$interface_pos)) stop("The coordinate ", current_coord, ": the 'interface_pos' argument should be numeric!")
    }
    
    current_coord = current_coord + 1
  }
}
