#' Function to smooth a bathymetry matrix
#'
#' @details
#'   Takes a matrix and applies smoothing algorithms, including options to
#'   maintain the depth of certain designated points.
#'   
#'   Two phases are used in the function: 'global' smoothing where a window is
#'   moved over the matrix and steep slopes are smoothed and (optionally) a
#'   minimum depth is set, without maintaining the designated points. Then a
#'   'local' smoothing is done starting from each designated point and the
#'   globally-smoothed bathymetry is adjusted to maintain the depths of the 
#'   points and still fulfill the smoothing criteria. Either step is optional.
#'   
#'   Smoothing is foremost an iterative process. Start with a certain parameter
#'   set and plot the results using 'plot_bathy' and then adjust. The common workflow
#'   is: 'read_bathy_as_matrix', (iteratively) 'smooth_bathy_matrix' & 'plot_bathy', then
#'   'add_bathy_to_ncdf'.
#' @param mtrx  matrix; numeric matrix, describing a bathymetry
#' @param maintain_coords data.table; should have columns 'ind_x' and 'ind_y', which
#'   should correspond to values in 'mtrx' that should not be affected by the smoothing.
#'   If NULL, no points are maintained and only 'global' smoothing is applied. Can be
#'   read from a csv with "x" and "y" coordinates using 'read_coord_maintain_file'
#' @param max_val numeric; maximum allowed value of 'method'. Should always be positive.
#' @param method character; 'diff' or 'rx0'. If 'diff', 'max_val' is interpreted as
#'   the maximum (absolute) difference between adjacent cells. If 'rx0', 'max_val' is
#'   the maximum slope parameter 'rx0', following Beckmann & Haidvogel (1993)
#' @param track_volume logical; if TRUE, the algorithm strives to maintain the original
#'   volume of the provided bathymetry. 
#' @param min_depth numeric; minimum allowed depth (always positive). Defaults to 0.0
#' @param global_smoothing logical; if TRUE, global smoothing is applied
#' @param box_width_global integer; width of box used in global smoothing
#' @param box_width_shallows integer; width of box used in global smoothing when assessing
#'   'min_depth'
#' @param adjust_factor_global numeric; [0-1] during global smoothing, in each step, the box
#'   is adjusted by this much towards the mean of the box. If 0, no adjustment happens, if 1,
#'   the whole box is set to the mean. 
#' @param max_iter_global integer; maximum amount of iteration in global smoothing
#' @param inflexibility numeric; vector of values between 1 and 0. Used during local smoothing.
#'   Describes the extent to which values around maintain_coords are smoothed. If you want to
#'   keep the areas around maintain_coords similar to that depth, provide more (high) numbers.
#'   Example: c(0.9, 0.7, 0.5, 0.3, 0.1). If NULL, no inflexibility is used.
#' @param max_scan_range integer; used during local smoothing. Longer range can prevent
#'   impossible constraints to occur, but a too long range can slow down the function.
#' @param max_vol_adj_step_local numeric; maximum allowed adjustment to the depth in order
#'   to correct the volume imbalance. Primary parameter to change if volume imbalance exists
#'   after smoothing. Not used if track_volume is FALSE.
#' @param bathy_round integer; optional parameter to round the bathymetry to this many digits
#' @param warn logical; throw a warning if some maintain_coords are shallower than 'min_depth'
#' @param margin numeric; low number to prevent errors in calculating volume imbalances and constraints
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
#' df_maintain = data.table(ind_x = 4, ind_y = 2) # mat[4,2] == 19.2
#' # Smooth using 'diff' and no inflexibility
#' smooth_bathy_matrix(mtrx = mat, maintain_coords = df_maintain,
#'                     max_val = 6, method = "diff",
#'                     max_vol_adj_step_local = 2.5)
#' # Smooth using 'rx0', min_depth, and inflexibility
#' smooth_bathy_matrix(mtrx = mat, maintain_coords = df_maintain,
#'                     max_val = 0.25, method = "rx0",
#'                     min_depth = 5.0,
#'                     inflexibility = c(0.5, 0.2),
#'                     max_vol_adj_step_local = 6.0)
#'  
#' @import ncdf4
#' @export

smooth_bathy_matrix = function(mtrx, maintain_coords, 
                        max_val,
                        method = "diff",
                        track_volume = TRUE,
                        min_depth = 0.0,
                        global_smoothing = TRUE,
                        box_width_global = 4L,
                        box_width_shallows = 6L,
                        adjust_factor_global = 0.05,
                        max_iter_global = 10L,
                        inflexibility = NULL,
                        max_scan_range = max(5L, length(inflexibility) * 2),
                        max_vol_adj_step_local = 0.5,
                        bathy_round = Inf,
                        warn = T,
                        margin = 1E-5){
  if(!(method %in% c("diff", "rx0"))){
    stop("'method' should be either 'diff' or 'rx0'!")
  }
  if(min_depth < 0){
    stop("'min_depth' should be provided as a positive number!")
  }
  
  if(any(mtrx < 0, na.rm = T)){
    stop("Negative values detected in the bathymetry!")
  }
  
  if(is.null(maintain_coords) & !global_smoothing){
    stop("Neither maintain coordinates nor global smoothing are selected, ",
         "which would cause no smoothing to be applied. Provide points ",
         "that should be maintained (local smoothing) and/or turn on ",
         "global smoothing to use this function.")
  }
  
  ##### Pre-loop calculations -----
  # Set limits of dimensions
  max_i = dim(mtrx)[1]
  max_j = dim(mtrx)[2]
  
  # Initial global smoothing (without maintain_coords)
  if(global_smoothing){
    bathy_glob = smooth_steep_slopes(mtrx, method, max_val = max_val, max_iter = max_iter_global,
                                     box_width = box_width_global, adjust_factor = adjust_factor_global)
    
    if(min(bathy_glob, na.rm = T) < min_depth){
      message("Depths shallower than 'min_depth' detected; extra smoothing step")
      bathy_glob = smooth_shallow_area(bathy_glob,
                                       min_depth = min_depth, max_iter = max_iter_global,
                                       box_width = box_width_shallows, adjust_factor = adjust_factor_global)
      bathy_glob = smooth_steep_slopes(bathy_glob, method,
                                       max_val = max_val, max_iter = max_iter_global,
                                       box_width = box_width_global, adjust_factor = adjust_factor_global)
    }
  }else{
    bathy_glob = mtrx
  }
  
  # Calculate volume imbalance
  if(track_volume){
    vol_imbal = sum(bathy_glob - mtrx, na.rm = T)
  }else{
    vol_imbal = 0.0
  }
  
  # Local smoothing (only if maintain_coords are provided)
  if(!is.null(maintain_coords)){
    ## Initial processing and checks
    maintain_coords = maintain_coords[!is.na(ind_x) & !is.na(ind_y)]
    maintain_coords = maintain_coords[ind_x >= 0 & ind_x <= max_i & ind_y >= 0 & ind_y <= max_j]
    
    for(i in seq_len(nrow(maintain_coords))){
      maintain_coords[i, orig_depth := mtrx[ind_x, ind_y]]
    }
    if(any(is.na(maintain_coords[, orig_depth]))){
      warning("Some coordinates in maintain_coord refer to NA values! These are ignored.")
    }
    maintain_coords = maintain_coords[!is.na(orig_depth)]
    
    # Warn if min_depth should be checked
    if(min_depth > 0 & warn){
      if(any(maintain_coords[, orig_depth] < min_depth)){
        warning("Some of the cells to be maintain have a shallower depth ",
                "than the 'min_depth' argument. These will not be adjusted.")
      }
    }
    
    ## Fill with maintain cells
    bathy_new = bathy_glob
    bathy_new[!is.na(bathy_new)] = -1 # This is code for "needs to be filled"
    bathy_origin = bathy_new
    for(i in seq_len(nrow(maintain_coords))){
      bathy_new[maintain_coords[i, ind_x], maintain_coords[i, ind_y]] = mtrx[maintain_coords[i, ind_x], maintain_coords[i, ind_y]]
      bathy_origin[maintain_coords[i, ind_x], maintain_coords[i, ind_y]] = i
    }
    
    ## Loop to expand from maintain cells, approximating bathy_glob
    while(sum(bathy_new == -1, na.rm = T) > 0L){
      # Loop over the matrix
      for(i in seq_len(max_i)){
        for(j in seq_len(max_j)){
          if(is.na(bathy_new[i, j])) next
          if(bathy_new[i, j] < 0) next
          
          # Left, up, right, down
          cell_orig = as.character(bathy_origin[i, j])
          
          # The function uses the parent environment
          environment(handle_cell) = environment()
          the_new_vals = handle_cell(i, j) # This function uses the global environment
          
          tmp_vol_imbal = 0.0
          
          if(!is.na(the_new_vals[1])){
            bathy_new[i-1, j] = the_new_vals[1]
            bathy_origin[i-1, j] = cell_orig
            tmp_vol_imbal = tmp_vol_imbal + the_new_vals[1] - bathy_glob[i-1, j]
          }
          if(!is.na(the_new_vals[2])){
            bathy_new[i, j+1] = the_new_vals[2]
            bathy_origin[i, j+1] = cell_orig
            tmp_vol_imbal = tmp_vol_imbal + the_new_vals[2] - bathy_glob[i, j+1]
          }
          if(!is.na(the_new_vals[3])){
            bathy_new[i+1, j] = the_new_vals[3]
            bathy_origin[i+1, j] = cell_orig
            tmp_vol_imbal = tmp_vol_imbal + the_new_vals[3] - bathy_glob[i+1, j]
          }
          if(!is.na(the_new_vals[4])){
            bathy_new[i, j-1] = the_new_vals[4]
            bathy_origin[i, j-1] = cell_orig
            tmp_vol_imbal = tmp_vol_imbal + the_new_vals[4] - bathy_glob[i, j-1]
          }
          
          # Update the volume imbalance
          if(track_volume){
            vol_imbal = vol_imbal + tmp_vol_imbal
          }
          
        }
      }
      
      message("Unfilled cells: ", sum(bathy_new == -1, na.rm = T))
      if(sum(bathy_new == -1, na.rm = T) == 0L) break
    }
  }else{
    bathy_new = bathy_glob
  }
  
  # Final volume check
  if(track_volume){
    if(abs(vol_imbal) > max_vol_adj_step_local + margin){
      message("Volume imbalance: ", vol_imbal, "; not fully resolved during smoothing. ",
              "Consider increasing 'max_vol_adj_step' or 'inflexibility' or other arguments that ",
              "allow the algorithm to correct volume.")
    }else{
      message("Total volume before and after smoothing are similar.")
    }
  }else{
    message("Volume change after smoothing: ", sum(bathy_new - mtrx, na.rm = T))
  }
  
  return(bathy_new)
}

#### Helper functions for smoothing

#' Handle local smoothing for one already-filled cell
#'
#' @details
#'   Uses the environment of the 'smooth_bathy_matrix' function
#' 
#' @param i,j  integer; indices of the cell to smooth

handle_cell = function(i, j){
  cell_orig = as.character(bathy_origin[i, j])
  
  new_vals = as.numeric(c(NA, NA, NA, NA)) # Left, up, right, down
  
  # The function uses the parent environment
  environment(handle_step) = environment()
  left = handle_step(i - 1, j, cell_orig)
  up = handle_step(i, j + 1, cell_orig)
  right = handle_step(i + 1, j, cell_orig)
  down = handle_step(i, j - 1, cell_orig)
  
  if(!is.na(left)){
    new_vals[1] = left
  }
  if(!is.na(up)){
    new_vals[2] = up
  }
  if(!is.na(right)){
    new_vals[3] = right
  }
  if(!is.na(down)){
    new_vals[4] = down
  }
  
  return(new_vals)
}

#' Handle local smoothing step for one empty cell
#'
#' @details
#'   Uses the environment of the 'handle_cell' function
#' 
#' @param new_i,new_j  integer; indices of the cell to smooth
#' @param cell_orig integer; corresponds to a cell in maintain_coords

handle_step = function(new_i, new_j, cell_orig){
  if(new_i < 1L | new_j < 1L | new_i > max_i | new_j > max_j){
    return(as.numeric(NA))
  }
  if(is.na(bathy_new[new_i, new_j])){
    return(as.numeric(NA))
  }
  if(bathy_new[new_i, new_j] > 0){
    return(as.numeric(NA))
  }
  
  ### Scan nearby values and based on this calculate what values are allowed
  df_scan = rbindlist(explore_matrix(bathy_new, new_i, new_j, max_scan_range))
  df_neighbours = df_scan[, .(lowest = min(value),
                              highest = max(value)),
                          by = distance]
  # What are possible/allowed values?
  if(method == "diff"){
    # Multiply distance with max_val
    allowed_maximum = min(sapply(seq_len(nrow(df_neighbours)),
                                 function(x) df_neighbours[x, distance] * max_val + df_neighbours[x, lowest]))
    allowed_minimum = max(sapply(seq_len(nrow(df_neighbours)),
                                 function(x) -df_neighbours[x, distance] * max_val + df_neighbours[x, highest]))
  }else if(method == "rx0"){
    # Call rx0_inv recursively for 'distance' times
    allowed_minimum = as.numeric(NA)
    allowed_maximum = as.numeric(NA)
    for(row in seq_len(nrow(df_neighbours))){
      temp_max = df_neighbours[row, lowest]
      temp_min = df_neighbours[row, highest]
      for(iter in seq_len(df_neighbours[row, distance])){
        temp_max = rx0_inv(temp_max, rx0 = max_val, sign = +1)
        temp_min = rx0_inv(temp_min, rx0 = max_val, sign = -1)
      }
      if(is.na(allowed_minimum) & is.na(allowed_maximum)){
        allowed_maximum = temp_max
        allowed_minimum = temp_min
      }else{
        if(temp_min > allowed_minimum){
          allowed_minimum = temp_min
        }
        if(temp_max < allowed_maximum){
          allowed_maximum = temp_max
        }
      }
    }
  }
  allowed_minimum = max(allowed_minimum, min_depth)
  
  if(allowed_maximum + margin < allowed_minimum){
    stop("Impossible constraints for cell [", new_i, ", ", new_j, "]! Consider ",
         "different function arguments or maintain points that are further apart.")
  }
  
  ### Calculate what value it should "optimally" have
  # This covers the provided "inflexibility" argument, but also any volume
  # adjustment that might need to happen. 
  orig_coord = c(maintain_coords[as.numeric(cell_orig), ind_x],
                 maintain_coords[as.numeric(cell_orig), ind_y])
  dist_from_orig = get_dist(c(new_i, new_j), orig_coord)
  change_factor = ifelse(dist_from_orig > length(inflexibility), 0, inflexibility[dist_from_orig])
  depth_diff = bathy_new[orig_coord[1], orig_coord[2]] - bathy_glob[orig_coord[1], orig_coord[2]]
  depth_optim = bathy_glob[new_i, new_j] + change_factor * depth_diff
  
  # Factor in volume imbalance
  if(abs(vol_imbal) > max_vol_adj_step_local + margin){
    depth_optim = depth_optim - sign(vol_imbal) * max_vol_adj_step_local / 4 # /4 because of potentially four directions
  }
  
  ### Set value
  if(depth_optim >= allowed_minimum & depth_optim <= allowed_maximum){
    depth_final = depth_optim
  }else if(depth_optim < allowed_minimum){
    depth_final = allowed_minimum
  }else if(depth_optim > allowed_maximum){
    depth_final = allowed_maximum
  }
  return(depth_final)
}

#' Recursive function that checks what values in 'mat' exist at what distance.
#' 
#' @param mat matrix; -1 are empty cells, NA not part of water, and other values are existing depths
#' @param start_i,start_j integer; coordinates of the reference cell
#' @param max_scan_range integer; the range over which the function should look for existing depths

explore_matrix = function(mat, start_i, start_j, max_scan_range){
  n_rows = nrow(mat)
  n_cols = ncol(mat)
  visited = matrix(FALSE, n_rows, n_cols)
  results = list()
  
  directions = list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))  # up, down, left, right
  
  queue = list(list(row = start_i, col = start_j, steps = 0))
  
  while(length(queue) > 0){
    current = queue[[1]]
    queue = queue[-1]
    
    row = current$row
    col = current$col
    steps = current$steps
    
    if(row < 1 || row > n_rows || col < 1 || col > n_cols) next
    if(visited[row, col] || is.na(mat[row, col]) || steps > max_scan_range) next
    
    visited[row, col] = TRUE
    
    if(mat[row, col] > 0){
      results[[length(results) + 1]] = list(distance = steps, value = mat[row, col])
    }
    
    if(mat[row, col] == -1 || (row == start_i && col == start_j)){
      for(dir in directions){
        new_row = row + dir[1]
        new_col = col + dir[2]
        queue[[length(queue) + 1]] = list(row = new_row, col = new_col, steps = steps + 1)
      }
    }
  }
  
  return(results)
}

#' Global smoothing function
#' 
#' @param bathy matrix; NA not part of water, and other values are existing depths
#' @param method character; 'diff' or 'rx0'
#' @param max_val numeric; maximum allowed value of 'method'
#' @param max_iter integer; maximum number of iterations
#' @param box_width integer; width of moving window
#' @param adjust_factor numeric; between 0 (no adjustment) and 1 (setting whole box
#'                      to the mean)
smooth_steep_slopes = function(bathy, method, max_val,
                               max_iter = 20L, box_width = 4L,
                               adjust_factor = 0.05){
  if(adjust_factor <= 0.0 | adjust_factor > 1.0){
    stop("'adjust_factor' must be between 0 and 1!")
  }
  
  # Set limits of dimensions
  max_i = dim(bathy)[1]
  max_j = dim(bathy)[2]
  box_width = min(box_width, max_i, max_j)
  
  ##### Iterations over matrix -----
  iter_num = 1L
  the_max_val = get_max_val(bathy, method)
  fixed_all = ifelse(the_max_val > max_val, F, T)
  
  while(iter_num <= max_iter & !fixed_all){
    message(paste0("Iteration ", iter_num, " - Max val = ", the_max_val))
    # Loop over the matrix
    for(i in seq_len(max_i)){
      if(i + box_width - 1 > max_i) next
      for(j in seq_len(max_j)){
        if(j + box_width - 1 > max_j) next
        box = bathy[i:(i + box_width - 1), j:(j + box_width - 1)]
        if(all(is.na(box))) next
        
        box_max_val = get_max_val(box, method)
        if(box_max_val < max_val){
          bathy[i:(i + box_width - 1), j:(j + box_width - 1)] = box
          next
        } 
        
        # Calculate mean of box
        mean_box = mean(box, na.rm = T)
        
        # Smooth while maintaining mean_box
        subtract_box = (box - mean_box) * adjust_factor
        subtract_box[is.na(subtract_box)] = 0
        box = box - subtract_box
        
        # Place smoothed version into bathy
        bathy[i:(i + box_width - 1), j:(j + box_width - 1)] = box
      }
    }
    
    # Update conditions
    the_max_val = get_max_val(bathy, method)
    fixed_all = ifelse(the_max_val > max_val, F, T)
    iter_num = iter_num + 1
  }
  
  return(bathy)
}

#' Global smoothing function for shallow areas
#' 
#' @param bathy matrix; NA not part of water, and other values are existing depths
#' @param min_depth numeric; minimum allowed depth
#' @param max_iter integer; maximum number of iterations
#' @param box_width integer; width of moving window
#' @param adjust_factor numeric; between 0 (no adjustment) and 1 (setting whole box
#'                      to the mean)
smooth_shallow_area = function(bathy, min_depth,
                               max_iter = 20L, box_width = 4L,
                               adjust_factor = 0.05){
  if(adjust_factor <= 0.0 | adjust_factor > 1.0){
    stop("'adjust_factor' must be between 0 and 1!")
  }
  
  # Set limits of dimensions
  max_i = dim(bathy)[1]
  max_j = dim(bathy)[2]
  box_width = min(box_width, max_i, max_j)
  
  ##### Iterations over matrix -----
  iter_num = 1L
  the_min_depth = min(bathy, na.rm = T)
  fixed_all = ifelse(the_min_depth <= min_depth, F, T)
  
  while(iter_num <= max_iter & !fixed_all){
    message(paste0("Iteration ", iter_num, " - Min depth = ", the_min_depth))
    # Loop over the matrix
    for(i in seq_len(max_i)){
      if(i + box_width - 1 > max_i) next
      for(j in seq_len(max_j)){
        if(j + box_width - 1 > max_j) next
        box = bathy[i:(i + box_width - 1), j:(j + box_width - 1)]
        if(all(is.na(box))) next
        
        box_min_d = min(box, na.rm = T)
        if(box_min_d >= min_depth | mean(box, na.rm = T) < min_depth) next
        
        # Calculate mean of box
        mean_box = mean(box, na.rm = T)
        
        # Smooth while maintaining mean_box and not changing maintain_grid
        subtract_box = (box - mean_box) * adjust_factor
        subtract_box[is.na(subtract_box)] = 0
        box = box - subtract_box
        
        # Place smoothed version into bathy
        bathy[i:(i + box_width - 1), j:(j + box_width - 1)] = box
      }
    }
    
    # Update conditions
    the_min_depth = min(bathy, na.rm = T)
    fixed_all = ifelse(the_min_depth <= min_depth, F, T)
    iter_num = iter_num + 1
  }
  
  return(bathy)
}

#' Get maximum value of 'method'
#' 
#' @param matrix matrix; NA not part of water, and other values are existing depths
#' @param method character; 'diff' or 'rx0'
get_max_val = function(matrix, method){
  the_max = 0.0
  for(i in seq_len(dim(matrix)[1])){
    for(j in seq_len(dim(matrix)[2])){
      if(method == "diff"){
        new_max = calc_max_diff(matrix, i, j)
      }else if(method == "rx0"){
        new_max = calc_max_rx0(matrix, i, j)
      }else{
        stop("Wrong method argument!")
      }
      
      if(!is.na(new_max)){
        if(new_max > the_max) the_max = new_max
      }
    }
  }
  return(the_max)
}

#' Calculate maximum difference with adjacent cells
#' 
#' @param matrix matrix; NA not part of water, and other values are existing depths
#' @param ind_x,ind_y integer; indices
calc_max_diff = function(matrix, ind_x, ind_y){
  val = matrix[ind_x, ind_y]
  if(!is.na(val)){
    return(max(0.0,
               abs(val - matrix[max(ind_x - 1, 0), ind_y]),
               abs(val - matrix[min(ind_x + 1, dim(matrix)[1]), ind_y]),
               abs(val - matrix[ind_x, max(ind_y - 1, 0)]),
               abs(val - matrix[ind_x, min(ind_y + 1, dim(matrix)[2])]),
               na.rm = T))
  }else{
    return(as.numeric(NA))
  }
}

#' Calculate rx0
#' 
#' @details
#'   delta-H / (2 * mean(H), Beckmann & Haidvogel 1993, doi:10.1175/1520-0485(1993)023<1736:NSOFAA>2.0.CO;2
#    part of equation 20b)
#' 
#' @param h1,h2 numeric; depths
rx0 = function(h1, h2){
  (h1 - h2) / (h1 + h2)
}

#' Calculate depth of h2, if you know h1 and rx0
#' 
#' @param h1 numeric; depth of a cell
#' @param rx0 numeric; slope parameter
#' @param sign numeric; if positive, increase in depth, if negative, decrease
rx0_inv = function(h1, rx0, sign = 1){
  if(sign > 0){
    return((rx0 + 1) * h1 / (1 - rx0))
  }else{
    return((1 - rx0) * h1 / (rx0 + 1))
  }
}

#' Calculate maximum rx0 between adjacent cells
#' 
#' @param matrix matrix; NA not part of water, and other values are existing depths
#' @param ind_x,ind_y integer; indices
calc_max_rx0 = function(matrix, ind_x, ind_y){
  val = matrix[ind_x, ind_y]
  if(!is.na(val)){
    return(max(0.0,
               abs(rx0(val, matrix[max(ind_x - 1, 0), ind_y])),
               abs(rx0(val, matrix[min(ind_x + 1, dim(matrix)[1]), ind_y])),
               abs(rx0(val, matrix[ind_x, max(ind_y - 1, 0)])),
               abs(rx0(val, matrix[ind_x, min(ind_y + 1, dim(matrix)[2])])),
               na.rm = T))
  }else{
    return(as.numeric(NA))
  }
}

#' Get distance between two cells
#' 
#' @param point1,point2 integer; each argument is a vector '[row,col]'
get_dist = function(point1, point2){
  return(abs(point1[1] - point2[1]) + abs(point1[2] - point2[2]))
}
