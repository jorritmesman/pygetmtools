#' Calculate flow from a PyGETM output file (3D)
#'
#' @details
#'   Read PyGETM 3D output files into R and calculate total flow (summed over all
#'   depth layers) at an interface containing several cells at each timestep.
#' 
#' @param ncdfs  vector; vector of character name(s) of the output nc file(s)
#' @param interface list; list containing coordinates on the form list(direction, x, y),
#'   where direction is character ("x" or "y"), and x,y numeric, representing the
#'   x and y coordinate of the centre of the cell. Optional list arguments are "sign",
#'   which is default +1 (positive in increasing x or y) and "interface_pos", default
#'   +1 (the interface in the direction of direction*sign). Can be visualised with
#'   function 'plot_interface'.
#' @author Jorrit Mesman, Henrik Jonasson, Alberto Fernandez Comesana, Moutushi Sen
#' @examples
#'  \dontrun{
#'     flux_interface(ncdfs = c("output_file1.nc", "output_file2.nc"),
#'                    interface = list(list(x = 652650, y = 6592050, direction = "x"),
#'                                     list(x = 652650, y = 6591750, direction = "x")))
#'  }
#' @import ncdf4
#' @export

flux_interface = function(ncdfs, interface){
  # Future potential expansions: extract profiles and variables other than discharge
  
  # Check interface arguments
  check_interface_arg(interface)
  all_dirs = unique(sapply(interface, function(x) x[["direction"]]))
  
  # Time series for all successive files
  lst_flow_files = list()
  
  for(file_name in ncdfs){
    ### Open and use nc file
    nc = nc_open(file_name)
    
    # Ensure that nc contains all required variables
    all_vars = names(nc$var)
    req_vars = "hnt"
    if("x" %in% all_dirs) req_vars = append(req_vars, "uk")
    if("y" %in% all_dirs) req_vars = append(req_vars, "vk")
    if(any(req_vars %notin% all_vars)){
      nc_close(nc)
      stop("Missing variables in output: ", req_vars[req_vars %notin% all_vars])
    }
    
    # Extract variables
    x_coord = ncvar_get(nc, "xt")[,1]
    xu_coord = ncvar_get(nc, "xu")[,1]
    y_coord = ncvar_get(nc, "yt")[1,]
    yv_coord = ncvar_get(nc, "yv")[1,]
    if("uk" %in% req_vars) uk = ncvar_get(nc, "uk")
    if("vk" %in% req_vars) vk = ncvar_get(nc, "vk")
    hnt = ncvar_get(nc, "hnt")
    time_vals = ncvar_get(nc, "time")
    time_att = ncatt_get(nc, "time")
    dates = convert_nc_time(time_vals, time_att)
    nc_close(nc)
    
    # Add missing time dimension if needed
    added_dimension = FALSE
    if(length(dim(hnt)) == 3L){
      dim(hnt) = c(dim(flow), 1)
      if(exists("uk")) dim(uk) = c(dim(uk), 1)
      if(exists("vk")) dim(vk) = c(dim(vk), 1)
      added_dimension = TRUE
    }
    
    # Get cell width
    width = abs(x_coord[2] - x_coord[1])
    if(abs(y_coord[2] - y_coord[1]) != width){
      stop("'x' and 'y' width seem to be different, and this function cannot be applied!")
    }
    
    ### Loop over coordinates in the interface
    lst_tmp = list()
    current_coord = 1
    for(coordinate in interface){
      # Check that the coordinate is within the domain
      if(coordinate$x < min(x_coord) | coordinate$x > max(x_coord) |
         coordinate$y < min(y_coord) | coordinate$y > max(y_coord)){
        stop("Coordinate ", current_coord, " is outside the range of coordinates in the output file:\\n",
                    "x [", min(x_coord), " - ", max(x_coord), "], y [", min(y_coord), " - ", max(y_coord), "]")
      }
      
      # Add default arguments for cleaner handling afterwards
      if(is.null(coordinate$sign)) coordinate$sign = 1 # >0 or <0 is important
      if(is.null(coordinate$interface_pos)) coordinate$interface_pos = 1 # >0 or <0 is important
      
      # Get total flow at current cell, and compile into list
      # target_x/y; get interface in increasing x/y direction, unless specified differently by "sign" and "interface_pos"
      if(coordinate$direction == "x"){
        flow = uk
        target_x = coordinate$x + sign(coordinate$sign) * sign(coordinate$interface_pos) * width / 2
        x_index = get_ind(target_x, xu_coord)
        y_index = get_ind(coordinate$y, y_coord)
      }else if(coordinate$direction == "y"){
        flow = vk
        target_y = coordinate$y + sign(coordinate$sign) * sign(coordinate$interface_pos) * width / 2
        x_index = get_ind(coordinate$x, x_coord)
        y_index = get_ind(target_y, yv_coord)
      }else{
        stop("The coordinate ", current_coord, ": 'direction' should be 'x' or 'y'")
      }
      
      # Slice at x-, y- indices to get flow and cell depth at a point, over depth and time 
      flow_point = flow[x_index, y_index, ,]
      hnt_point = hnt[x_index, y_index, ,]
      # Check if interfaces exist at given coordinates
      if(all(is.na(flow_point))){
        stop("The coordinate ", current_coord, ": flow is all NA. Check if x-, y-coordinates matches cell with existing interface in chosen direction")
      }
      
      # Calculate discharge
      # Multiply flow with height element-wise, sum along the layer axis, then multiply with width
      # to get total discharge at each time step
      # Note: if extraction of profiles is enabled, this section will require adjustment
      if(!added_dimension){ 
        discharge = apply(flow_point * hnt_point, 2, sum) * width
      }
      else{
        # If no time axis present, need to use sum insted of apply
        discharge = sum(flow_point * hnt_point) * width
      }
      
      # Ensure that positive discharge is in direction of increasing x/y
      # (before applying user-defined sign). This is done "wrong" by the model
      # if coordinates are provided in decreasing order
      the_sign = ifelse(coordinate$direction == "y",
                        ifelse(all(diff(y_coord) > 0), 1, -1),
                        ifelse(all(diff(x_coord) > 0), 1, -1))
      discharge = the_sign * discharge
      
      # Now apply user-defined sign
      if(!is.null(coordinate$sign)){
        if(coordinate$sign < 0) discharge = -1 * discharge
      }
      
      # Add to list before summing all coordinates
      df_tmp = data.table(date = dates,
                          flow = discharge)
      df_tmp = df_tmp[!is.na(flow)]
      lst_tmp[[current_coord]] = df_tmp
      current_coord = current_coord + 1
    }
    
    df_file_flow = rbindlist(lst_tmp)
    df_file_flow = df_file_flow[, .(flow = sum(flow)), by = date]
    lst_flow_files[[length(lst_flow_files) + 1]] = df_file_flow
  }
  
  return(rbindlist(lst_flow_files))
}
