#' Extract part of a matrix, based on observations
#' 
#' @details
#'   Loops over a set of provided observations and extracts values for the
#'   closest locations and times. Only the closest cells are extracted, no interpolation
#'   is done. This function is for internal use in pair_sim_and_obs.R. 
#'
#' @param mtrx  matrix; matrix of the variable to extract. Result of ncdf4::ncvar_get(...)
#' @param mtrx_zft matrix: matrix of 'zft'. Result of ncdf4::ncvar_get(...)
#' @param x_dim,y_dim numeric; vector of the values of the 'x' and 'y' dimensions
#' @param time_dim POSIXct; vector of the values of the 'time' dimension in the nc file
#' @param col_x,col_y,col_depth character; column names in 'observations' for the dimensions
#' @param sim_date character; column name of the simulation date
#' @param save_z logical; Whether to save the z-layer from which simulation value was extracted.
#'   Can be used to average over if you want to avoid multiple observations linking to the same z-layer.
#' @author
#'   Jorrit Mesman

slice_matrix_with_obs = function(mtrx, mtrx_zft,
                                 x_dim, y_dim, time_dim,
                                 observations,
                                 col_x = "x", col_y = "y", sim_date = "sim_date",
                                 col_depth = "depth", save_z = TRUE){
  
  observations[, sim := as.numeric(NA)]
  
  if(save_z){
    observations[, z := as.integer(NA)]
  }
  
  colind_sim = which(names(observations) == "sim")
  colind_z = which(names(observations) == "z")
  
  for(i in seq_len(nrow(observations))){
    # Get x, y, and time indices
    x_ind = get_ind(observations[i, ..col_x][[1]], x_dim)
    y_ind = get_ind(observations[i, ..col_y][[1]], y_dim)
    t_ind = get_ind(observations[i, ..sim_date][[1]], time_dim)
    
    # Get z index
    zft_vals = mtrx_zft[x_ind, y_ind,, t_ind]
    zft_vals = zft_vals - max(zft_vals) # Surface should always be at 0, because
                                        # we assume that observations are relative
                                        # to the surface level!
    
    z_ind = findInterval(observations[i, ..col_depth][[1]], zft_vals)
    if(z_ind == 0L) next # Observation deeper than wlvl
    
    set(observations, i = i, j = colind_sim, value = mtrx[x_ind, y_ind, z_ind, t_ind])
    
    if(save_z){
      set(observations, i = i, j = colind_z, value = z_ind)
    }
  }
  
  return(observations)
}
