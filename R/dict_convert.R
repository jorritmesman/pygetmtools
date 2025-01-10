#' Translate keys into values using a dictionary
#'
#' @details
#'   This function works akin to Python dictionaries and a dictionary (list) with
#'   unique keys should be provided. This function could probably be optimised
#'   further. Currently only works for character values.
#' 
#' @param vals character; values to be compared to the names of the dictionary 
#'   and converted.
#' @param dict list; values to be compared to the names of the dictionary 
#'   and converted. All names must be unique!
#' @param dict_lvl character; if NULL (default), then the value is assumed to be
#'   the value of the list. If not, the value can be one level "deeper" into the
#'   list (see example).
#' @param other character; value to assign if 'vals' to not occur in the names
#'   of the dictionary.
#' @examples
#'   countries = c("France", "Netherlands", "Hungary", "Germany")
#'   country_code_table = list(Netherlands = list(alpha2 = "NL",
#'                                                alpha3 = "NLD"),
#'                             France = list(alpha2 = "FR",
#'                                           alpha3 = "FRA"),
#'                             Germany = list(alpha2 = "DE",
#'                                            alpha3 = "DEU"),
#'                             Hungary = list(alpha2 = "HU",
#'                                            alpha3 = "HUN"),
#'                             UK = list(alpha2 = "GB",
#'                                       alpha3 = "GBR"),
#'                             Spain = list(alpha2 = "ES",
#'                                          alpha3 = "ESP"))
#'   dict_convert(countries, country_code_table, dict_lvl = "alpha3")
#' @export

dict_convert = function(vals, dict, dict_lvl = NULL, other = as.character(NA)){
  df_conv = data.table(vals = vals,
                       output = other)
  
  if(is.null(dict_lvl)){
    for(i in unique(df_conv$vals)){
      if(i %in% names(dict)){
        df_conv[vals == i, output := dict[[i]]]
      }
    }
  }else{
    for(i in unique(df_conv$vals)){
      if(i %in% names(dict)){
        df_conv[vals == i, output := dict[[i]][[dict_lvl]]]
      }
      
    }
  }
  
  return(df_conv$output)
}
