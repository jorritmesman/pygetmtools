#' Extract part of a matrix
#' 
#' @details
#'   Takes a matrix (as gotten from ncvar_get() on a PyGETM output file) and
#'   extracts part of it, based on the input arguments. 
#'   The dimensions of the matrix, in order, are 'x', 'y', 'z', 'time'
#'   At least for now, if specified, x, y, z, and depth must be single numbers:
#'   ranges are not possible. This function is for internal use in
#'   read_pygetm_output.R. 
#'
#' @param mtrx  matrix; matrix of the variable to extract. Result of ncdf4::ncvar_get(...)
#' @param x_dim,y_dim numeric; vector of the values of the 'x' and 'y' dimensions
#' @param x,y numeric; x and y coordinates. If NULL, extracts all.
#'   These are the actual values, not the index.
#' @param depth numeric; depth below surface, should be negative. If NULL, extracts all.
#'   'depth' and 'z' cannot be provided both.
#' @param z numeric; layer number (1 at bottom). If NULL, extracts all.
#'   Note: NOT in Python-counting (so first one is "1"). 'depth' and 'z' cannot be provided both. 
#' @param transect data.table; headers 'x' and 'y', providing a set of cells to extract.
#'   Can be used in combination with depth/z or not. 'If NULL, 'x' and 'y' args are
#'   ignored of 'transect' is provided. If NULL, will not extract transect.
#' @param mtrx_zct matrix: matrix of 'zct'. Result of ncdf4::ncvar_get(...)
#' @param mtrx_surf,mtrx_bott matrix; similar to mtrx_zct, but based on a matrix
#'   of the interfaces ('zft') and given for the uppermost and lowermost layer, respectively
#' @param add_depth_to_output logical; if true and 'depth' is not provided, then
#'   still 'depth' is calculated for each value of 'z'
#' @author
#'   Jorrit Mesman

slice_matrix = function(mtrx, x_dim, y_dim, x, y, depth, z, transect, mtrx_zct = NULL,
                        mtrx_surf = NULL, mtrx_bott = NULL,
                        add_depth_to_output = T){
  m_dims = dim(mtrx)
  
  # Separate approaches transects and others
  if(is.null(transect)){
    # Build dictionary to convert x and y indices to coordinates
    dict_x = as.list(x_dim)
    names(dict_x) = as.character(seq_len(length(x_dim)))
    dict_y = as.list(y_dim)
    names(dict_y) = as.character(seq_len(length(y_dim)))
    
    # x & y
    if(is.null(x)){
      x_extent = seq_len(m_dims[1])
    }else{
      x_extent = get_ind(x, x_dim)
    }
    if(is.null(y)){
      y_extent = seq_len(m_dims[2])
    }else{
      y_extent = get_ind(y, y_dim)
    }
    if(is.null(z)){
      z_extent = seq_len(m_dims[3])
    }else{
      z_extent = z
    }
    
    # Using this apply-function ensures that the result can be written as a data.table
    if(any(!is.null(x), !is.null(y), !is.null(z))){
      lst_mtrx = lapply(seq_len(m_dims[4]), function(t){
        tmp_df = data.table(mtrx[x_extent,
                                 y_extent,
                                 z_extent,
                                 t])
        setnames(tmp_df, as.character(y_extent))
        tmp_df[, `:=`(x_ind = x_extent,
                      z = z_extent,
                      time_ind = t)]
        tmp_df
      })
    }else{
      lst_mtrx = lapply(seq_len(m_dims[4]), function(t){
        lst_mtrx_z = lapply(seq_len(m_dims[3]), function(z){
          tmp_df = data.table(mtrx[x_extent,
                                   y_extent,
                                   z,
                                   t])
          setnames(tmp_df, as.character(y_extent))
          tmp_df[, `:=`(x_ind = x_extent,
                        z = z,
                        time_ind = t)]
          tmp_df
        })
        tmp_df_z = rbindlist(lst_mtrx_z)
      })
    }
    
    df_var = rbindlist(lst_mtrx)
    df_var = melt(df_var, id.vars = c("x_ind", "z", "time_ind"), variable.factor = F,
                  variable.name = "y_ind", value.name = "val")
    df_var[, y_ind := as.numeric(y_ind)]
    
    # Go from indices to coordinates
    df_var[, `:=`(x = dict_convert(x_ind, dict_x),
                  y = dict_convert(y_ind, dict_y))]
    setorder(df_var, time_ind, x, y, z)
  }else{
    transect[, id := seq_len(.N)]
    if(is.null(z)){
      z_extent = seq_len(m_dims[3])
    }else{
      z_extent = z
    }
    lst_mtrx = lapply(seq_len(nrow(transect)), function(id) data.table(transect_id = id,
                                                                       x = transect[id, x],
                                                                       y = transect[id, y],
                                                                       z = z_extent,
                                                                       time_ind = seq_len(m_dims[4]),
                                                                       val = mtrx[get_ind(transect[id, x], x_dim),
                                                                                  get_ind(transect[id, y], y_dim),
                                                                                  z_extent,]))
    df_var = rbindlist(lst_mtrx)
    setorder(df_var, time_ind, x, y, z)
  }
  
  # Any missing grid cell can be assumed to be NA in further analyses
  df_var = df_var[!is.na(val)]
  if(nrow(df_var) == 0L){
    message("No data on this location.")
    return(df_var)
  }
  
  if(!is.null(depth) | add_depth_to_output){
    # Find the zct values for the grids in df_var
    zct_vals = m_zct[as.matrix(df_var[, .(x_ind, y_ind, z, time_ind)])]
    df_var[, zct := zct_vals]
    rm(zct_vals)
  }
  
  if(!is.null(depth)){
    # Add surface and bottom levels
    z_surf_vals = m_lvl_surf[as.matrix(df_var[, .(x_ind, y_ind, time_ind)])]
    z_bott_vals = m_lvl_bott[as.matrix(df_var[, .(x_ind, y_ind, time_ind)])]
    df_var[, `:=`(z_surf = z_surf_vals,
                  z_bott = z_bott_vals)]
    rm(z_surf_vals, z_bott_vals)
    
    # Calculate z_below_surface
    df_var[, `:=`(depth_rel_surf = zct - z_surf,
                  depth_bott = z_bott - z_surf,
                  zct = NULL,
                  z_surf = NULL,
                  z_bott = NULL)]
    
    # Extract value for specified depth
    df_var = df_var[, .(depth = depth,
                        val = extract_from_profile(vals = val,
                                                   depths = depth_rel_surf,
                                                   the_depth = depth,
                                                   depth_bott = unique(depth_bott))),
                    by = .(time_ind, x, y)]
  }
  
  # Fix headers
  if("zct" %in% names(df_var) & !("depth" %in% names(df_var))){
    setnames(df_var, old = "zct", new = "depth")
  }
  
  cols_to_keep = c("time_ind", "x", "y")
  if("z" %in% names(df_var)) cols_to_keep = c(cols_to_keep, "z")
  if("depth" %in% names(df_var)) cols_to_keep = c(cols_to_keep, "depth")
  cols_to_keep = c(cols_to_keep, "val")
  
  df_var = df_var[, ..cols_to_keep]
  setorder(df_var, "x", "y", "time_ind")
  
  return(df_var)
}
