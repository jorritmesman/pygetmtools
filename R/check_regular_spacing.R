#' Check whether depth values are regularly spaced
#' 
#' Checks if for each unique combination of 'id' and 'date', depths are regularly spaced
#' 
#' @param df  data.table
#' @param col_date,col_id,col_depth character; column names, 'col_depth' is grouped by 'col_date' and 'col_id'.

check_regular_spacing = function(df, col_date = "date", col_id = col_id, col_depth = col_depth){
  tmp = df[, .(spacings = length(unique(diff(get(col_depth))))),
     by = c(col_date, col_id)]
  ifelse(all(tmp$spacings == 1L), TRUE, FALSE)
}
