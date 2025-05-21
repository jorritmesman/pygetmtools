#' Create a PyGETM inflow netcdf file
#' 
#' @description
#' Create a netcdf inflow file that can be interpreted with the 'pygetm.input.from_nc' python function
#' 
#' @details
#' Important: any flow constituent that is added needs to have been
#' initialised in the FABM yaml file and it's assumed to be the same name
#' in the header as in the model; you can add a fabm.yaml file to check.
#' 
#' The time series added can be, but do not have to be continuous; the script
#' will create a continuous series using the 'timestep' argument and linear
#' interpolation.
#' 
#' Note that this script does not add units to the created netcdf file. Ensure
#' that the unit of the input is the same as that is used by PyGETM. For tracers,
#' the unit may be arbitrary, but PyGETM will use the same unit as is in the inflow
#' 
#' @param filename character; Point to csv file with headers ('date', 'flow', and any constituents).
#' 'date' needs to have format 'yyyy-mm-dd hh:mm:ss' and 'flow' must be in m3/s
#' @param lat,lon numeric; latitude and longitude in decimal degrees (north/east)
#' @param file_out character; path to output netcdf file
#' @param fabmyaml character; path to fabm.yaml file used for simulation. If provided, will check the constituents in the file
#' @param timestep character; interpretable by seq.POSIXct
#' @param origin_getm character; 'yyyy-mm-dd hh:mm:ss' format, time origin used in GETM nc file
#' @param plot logical; if true, makes a plot of the output that was written
#' @author
#' Jorrit Mesman
#' @examples
#'  \dontrun{
#'   # "inflow_tracer.csv" has columns "date", "flow", and "tracer_c"
#'   # "tracer_c" is present in a fabm.yaml file associated with a PyGETM simulation
#'   create_inflow_nc_from_csv(filename = "inflow_tracer.csv",
#'   lat = 62.65,
#'   lon = -97.79,
#'   file_out = "Tracer_file_1.nc",
#'   timestep = "1 day")
#'  }
#' @import ncdf4
#' @import lubridate
#' @import yaml
#' @import ggplot2
#' @import ggpubr
#' @export

create_inflow_nc_from_csv = function(filename, lat, lon, file_out,
                                     fabmyaml = NULL, timestep = "1 hour",
                                     origin_getm = "1900-01-01 00:00:00",
                                     plot = FALSE){
  # Read file
  df = fread(filename)
  
  # Check validity of filename
  if(!("date" %in% names(df) & "flow" %in% names(df))){
    stop("'filename' needs to have, at least, a column named 'date' and a column ",
         "named 'flow'!")
  }
  
  if(any(duplicated(df$date))){
    stop("Duplicated dates detected in 'filename'! If you want to add sudden changes ",
         " in the file, ensure that this works with the 'timestep' argument to ",
         "this function.")
  }
  
  # If other columns are present; check. Also uses the fabmyaml argument, if given.
  if(length(names(df) > 2)){
    col_names = names(df)[!(names(df) %in% c("date", "flow", "temp", "salt"))]
    for(i in col_names){
      spl = strsplit(i, "_")[[1]]
      if(length(spl) == 1L){
        stop("Column name '", i, "' is not subsetted with '_'! All tracer variables ",
             "should be defined as 'modelname_variable'. For instance, 'tracer_c' or ",
             "'selmaprotbas_po'. See the fabm.yaml file used for the simulation and ",
             "the variable that you are targeting; write the value of the model (lvl 2), then ",
             "an underscore, and then the name of the variable which is listed under ",
             "'initialization'.")
      }
    }
    
    if(!is.null(fabmyaml)){
      wrong_cols = c()
      fabm = read_yaml(fabmyaml)
      for(i in col_names){
        spl = strsplit(i, "_")[[1]]
        entry = fabm[["instances"]][[spl[1]]][["initialization"]][[spl[2]]]
        if(is.null(entry)) wrong_cols = append(wrong_cols, i)
      }
      if(length(wrong_cols) > 0L){
        stop("Could not find the following variables in fabmyaml:\n",
             paste(wrong_cols, collapse = ", "))
      }
    }
  }
  
  # Create a full, continuous time series
  df[, date := as.POSIXct(date, tz = "UTC")]
  full_times = seq.POSIXt(df[1L, date], df[.N, date], by = timestep)
  
  df_write = data.table(all_times = full_times)
  for(i in names(df)[names(df) != "date"]){
    df_write[, (i) := approx(df[["date"]], df[[i]], xout = all_times)$y]
  }
  
  # Create netcdf
  times_nc = as.numeric(difftime(df_write$all_times, as.POSIXct(origin_getm), units = "hours"))
  timedim = ncdim_def("time", units = paste0("hours since ", origin_getm), vals = times_nc, unlim = T)
  lon_dim = ncdim_def("longitude", "degrees_east", vals = ifelse(lon >= 0, lon, lon + 360), longname = "longitude")
  lat_dim = ncdim_def("latitude", "degrees_north", vals = lat, longname = "latitude")
  
  vars = names(df_write)[-1]
  var_list = vector("list", length = length(vars))
  for(i in seq_along(vars)){
    the_unit = fifelse(vars[i] == "flow", "m3/s", "-")
    nc_var = ncvar_def(vars[i], the_unit, list(lon_dim, lat_dim, timedim),
                       longname = vars[i], prec = "float", compression = 5L, shuffle = FALSE)
    var_list[[i]] = nc_var
  }
  
  ncout = nc_create(file_out, var_list, force_v4 = T)
  for(i in vars){
    ncvar_put(ncout, i, df_write[[i]])
  }
  nc_close(ncout)
  
  if(plot){
    plts = list()
    for(i in seq_along(vars)){
      plts[[i]] = ggplot(df_write) +
        geom_line(aes(all_times, .data[[vars[i]]])) +
        labs(x = "date") +
        theme_light()
    }
    ggarrange(plotlist = plts, nrow = length(vars), align = "h")
  }
}
