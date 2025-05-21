#' Converts netcdf time to POSIXct
#' 
#' Converts netcdf time from numerical values to POSIXct, when providing values and attributes. The
#' values are the result of ncvar_get(nc, "time") and the attribute of ncatt_get(nc, "time")
#' 
#' @param time_vals numeric; returned by ncdf4::ncvar_get()
#' @param time_att list; returned by ncdf4::ncatt_get().
#' @param tz character; time zone, accepted by as.POSIXct. Defaults to UTC
#' @export

convert_nc_time = function(time_vals, time_att, tz = "UTC"){
  tustr = strsplit(time_att$units, " ")
  step = tustr[[1]][1]
  origin = as.POSIXct(paste(tustr[[1]][3], tustr[[1]][4]),
                      format = "%Y-%m-%d %H:%M:%S", tz = tz)
  multiplier = fcase(step == "days", 24 * 60 * 60,
                     step == "hours", 60 * 60,
                     step == "minutes", 60,
                     step == "seconds", 1)
  return(as.POSIXct(time_vals * multiplier, origin = origin, tz = "UTC"))
}
