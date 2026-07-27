#' Extract part of a matrix
#' 
#' @details
#'   Takes a list of matrices (as gotten from ncvar_get() on a PyGETM local output file) and
#'   extracts part of them, based on the input arguments. This function is for internal use in
#'   read_local.R. 
#'
#' @param lst_mtrx  list; list of matrices of the variables to extract. Result of ncdf4::ncvar_get(...)
#' @param depth numeric; depth below surface, should be negative. If NULL, extracts all.
#'   'depth' and 'z' cannot be provided both.
#' @param z numeric; layer number (1 at bottom). If NULL, extracts all.
#'   Note: NOT in Python-counting (so first one is "1"). 'depth' and 'z' cannot be provided both. 
#' @param mtrx_zct matrix: matrix of 'zct'. Result of ncdf4::ncvar_get(...)
#' @param mtrx_surf,mtrx_bott matrix; similar to mtrx_zct, but based on a matrix
#'   of the interfaces ('zft') and given for the uppermost and lowermost layer, respectively
#' @param add_depth_to_output logical; if true and 'depth' is not provided, then
#'   still 'depth' is calculated for each value of 'z'
#' @author
#'   Jorrit Mesman

slice_matrix_local = function(lst_mtrx, depth, z, mtrx_zct = NULL,
                              mtrx_surf = NULL, mtrx_bott = NULL,
                              add_depth_to_output = T){
  m_dims = dim(lst_mtrx[[1]])
  
  if(is.null(z)){
    z_extent = seq_len(m_dims[1])
  }else{
    z_extent = z
  }
  
  # Using this apply-function ensures that the result becomes a data.table
  if(!is.null(z)){
    df_var = data.table(time_ind = seq_len(m_dims[2]),
                        z = z_extent)
    for(var_name in names(lst_mtrx)){
      df_var[, (var_name) := lst_mtrx[[var_name]][z_extent, ]]
    }
  }else{
    lst_tmp = lapply(seq_len(m_dims[2]), function(x){
      df_temp = data.table(time_ind = x,
                           z = z_extent)
      for(var_name in names(lst_mtrx)){
        df_temp[, (var_name) := lst_mtrx[[var_name]][, x]]
      }
      df_temp
    })
    df_var = rbindlist(lst_tmp)
  }
  
  # Any missing grid cell can be assumed to be NA in further analyses
  var_cols = names(df_var)[names(df_var) %notin% c("time_ind", "z")]
  keep = if(length(var_cols) == 1){
    !is.na(df_var[[var_cols]])
  }else{
    df_var[, rowSums(!is.na(.SD)) > 0, .SDcols = var_cols]
  }
  df_var = df_var[keep]
  if(nrow(df_var) == 0L){
    message("No data on this location.")
    return(df_var)
  }
  
  if(!is.null(depth) | add_depth_to_output){
    # Find the zct values for the grids in df_var
    zct_vals = mtrx_zct[as.matrix(df_var[, .(z)])]
    df_var[, zct := zct_vals]
    rm(zct_vals)
  }
  
  if(!is.null(depth)){
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
    
    # Extract value for specified depths
    the_depths = depth
    df_var = df_var[, {vals_out = extract_from_profile(vals = .SD,
                                                       depths = depth_rel_surf,
                                                       depths_out = the_depths,
                                                       depth_bott = unique(depth_bott))
    as.data.table(vals_out)[, depth := the_depths][]},
    by = time_ind,
    .SDcols = names(lst_mtrx)]
  }
  
  # Second time removing NA values
  var_cols = names(df_var)[names(df_var) %notin% c("time_ind", "z", "depth", "zct")]
  keep = if(length(var_cols) == 1){
    !is.na(df_var[[var_cols]])
  }else{
    df_var[, rowSums(!is.na(.SD)) > 0, .SDcols = var_cols]
  }
  df_var = df_var[keep]
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
  cols_to_keep = c(cols_to_keep, names(lst_mtrx))
  
  df_var = df_var[, ..cols_to_keep]
  setorder(df_var, "time_ind")
  
  return(df_var)
}
