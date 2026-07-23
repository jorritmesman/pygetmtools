#' Extract part of a matrix
#' 
#' @details
#'   Takes a matrix (as gotten from ncvar_get() on a PyGETM local output file) and
#'   extracts part of it, based on the input arguments. This function is for internal use in
#'   read_local.R. 
#'
#' @param mtrx  matrix; matrix of the variable to extract. Result of ncdf4::ncvar_get(...)
#' @param depth numeric; depth below surface, should be negative. If NULL, extracts all.
#'   'depth' and 'z' cannot be provided both.
#' @param z numeric; layer number (1 at bottom). If NULL, extracts all.
#'   Note: NOT in Python-counting (so first one is "1"). 'depth' and 'z' cannot be provided both. 
#' @param mtrx_zct matrix: matrix of 'zct'. Result of ncdf4::ncvar_get(...)
#' @param mtrx_surf,mtrx_bott matrix; similar to mtrx_zct, but based on a matrix
#'   of the interfaces ('zft') and given for the uppermost and lowermost layer, respectively
#' @param add_depth_to_output logical; if true and 'depth' is not provided, then
#'   still 'depth' is calculated for each value of 'z'
#' @param profile_interval numeric; single value, calculates output depths for a profile with
#'   this depth interval
#' @author
#'   Jorrit Mesman

slice_matrix_local = function(mtrx, depth, z, mtrx_zct = NULL,
                              mtrx_surf = NULL, mtrx_bott = NULL,
                              add_depth_to_output = T, profile_interval = NULL){
  m_dims = dim(mtrx)
  
  if(is.null(z)){
    z_extent = seq_len(m_dims[1])
  }else{
    z_extent = z
  }
  
  # Using this apply-function ensures that the result becomes a data.table
  if(!is.null(z)){
    df_var = data.table(time_ind = seq_len(m_dims[2]),
                        z = z_extent,
                        val = mtrx[z_extent, ])
  }else{
    lst_mtrx = lapply(seq_len(m_dims[2]), function(x){
      data.table(time_ind = x,
                 z = z_extent,
                 val = mtrx[, x])
    })
    df_var = rbindlist(lst_mtrx)
  }
  
  # Any missing grid cell can be assumed to be NA in further analyses
  df_var = df_var[!is.na(val)]
  if(nrow(df_var) == 0L){
    message("No data on this location.")
    return(df_var)
  }
  
  if(!is.null(depth) | !is.null(profile_interval) | add_depth_to_output){
    # Find the zct values for the grids in df_var
    zct_vals = mtrx_zct[as.matrix(df_var[, .(z)])]
    df_var[, zct := zct_vals]
    rm(zct_vals)
  }
  
  if(!is.null(depth) | !is.null(profile_interval)){
    # Add surface and bottom levels
    z_surf_vals = mtrx_surf[as.matrix(df_var[, .(time_ind)])]
    z_bott_vals = mtrx_bott[as.matrix(df_var[, .(time_ind)])]
    df_var[, `:=`(z_surf = z_surf_vals,
                  z_bott = z_bott_vals)]
    rm(z_surf_vals, z_bott_vals)
    
    # Calculate depth_below_surface
    df_var[, `:=`(depth_rel_surf = zct - z_surf,
                  depth_bott = z_bott - z_surf,
                  zct = NULL,
                  z_surf = NULL,
                  z_bott = NULL)]
    
    # Extract value for specified depth
    if(!is.null(depth)){
      the_depths = depth
    }else if(!is.null(profile_interval)){
      the_depths = seq(0, min(df_var$depth_bott), by = -abs(profile_interval))
    }
    
    df_var = df_var[, .(depth = the_depths,
                        val = extract_from_profile(vals = val,
                                                   depths = depth_rel_surf,
                                                   depths_out = the_depths,
                                                   depth_bott = unique(depth_bott))),
                    by = time_ind]
    
  }
  
  # Second time removing NA values
  df_var = df_var[!is.na(val)]
  if(nrow(df_var) == 0L){
    message("No data on this location.")
    return(df_var)
  }
  
  # Fix headers
  if("zct" %in% names(df_var) & !("depth" %in% names(df_var))){
    setnames(df_var, old = "zct", new = "depth")
  }
  
  cols_to_keep = c("time_ind")
  if("z" %in% names(df_var)) cols_to_keep = c(cols_to_keep, "z")
  if("depth" %in% names(df_var)) cols_to_keep = c(cols_to_keep, "depth")
  cols_to_keep = c(cols_to_keep, "val")
  
  df_var = df_var[, ..cols_to_keep]
  setorder(df_var, "time_ind")
  
  return(df_var)
}
