# This is an example script to demonstrate how to extract and process
# PyGETM output files with pygetmtools

library(pygetmtools)

# For additional exploration or plotting, also import these packages:
library(ncdf4)
library(ggplot2)
library(data.table)

##### The output file(s) -----
# In this example, we assume that the output is split over multiple files,
# but if all your output is in one file, you can instead provide one file.

### USER-INPUT REQUIRED
# You need to change this to point to the output file(s) and bathymetry file
# on your computer. This would include the path to that file (e.g. path/to/folder/output.nc)
# Output file(s)
output_ncs = c("output2001.nc", "output2002.nc")

# Bathymetry file
bathy_nc = "bathymetry.nc"
depth_name = "bathymetry" # Name of the depth variable in the bathymetry file

##### General information -----
# "pygetmtools" supports more functions than those demonstrated here, such as
# smoothing bathymetries, calculating discharges within the simulated domain, and
# creating netcdf inflow files from csv files. Each function has a help file
# associated to it that helps you to understand and run the function.

# List all functions in the package, with links to their help files
help(package = pygetmtools)

# In the help files, you get more information on what each function does and
# what inputs it requires. You can read a specific help file also by typing ?function_name
?explore_nc

##### Listing available output -----
# List all the output variables and dimensions that are written to the file
explore_nc(output_ncs[1])
# In the following examples, we extract temperature ("temp"), but you could apply
# these functions for any variable in the output

# You can get some additional information about individual variables for example by:
explore_nc(output_ncs[1], var = "temp")

##### Plotting and exploring the lake bathymetry -----
# You can interactively plot the bathymetric information - hover over the cells to see the depth
# "bathymetry" is the default name of the depth variable, but might be different in your file
# For the axes, you can plot as "projected" (PyGETM's x & y values), "indices", or
# "geographic" (an approximation of latitude and longitude)
plot_interactive_bathymetry(bathy_nc, bathy_name = depth_name, coord_type = "projected")

##### Extracting output -----
# Example 1: get temperature ("temp") output for one specific location and depth
# This location could be for example be found with the plot_interactive_bathymetry function
df1 = read_multiple(output_ncs, var = "temp", x = 639000, y = 6584000, depth = -2)

# Example 2: get full profile for one specific location
df2 = read_multiple(output_ncs, var = "temp", x = 639000, y = 6584000)
# Note: the "profile_interval" argument let's you specify the interval of the depth variable

# Example 3: get surface (z = 19) values for the whole lake
# Note: the surface level is the highest "z" layer. In this case, it is read from df2
max_z = max(df2$z)
df3 = read_multiple(output_ncs, var = "temp", z = max_z)

##### Writing the output to csv -----
# You can use standard R functions to write these files to a csv, e.g.
write.csv(df1, file = "example_output_pygetmtools.csv")

# Note: by default this writes to your working directory. Provide the path
# to where you want to write this file

##### Plotting the output in R -----
# pygetmtools provides a few plotting options as well, which are demonstrated here.
# Firstly, currently, these functions do not plot the bathymetry as a geospatial
# object, so your domain may look stretched or compressed compared to the real 
# shape. For publication-ready plots, we would encourage to plot as a geospatial
# object, which can be done in R, but pygetmtools does not yet offer support for this.
# Secondly, we mostly provide these ready-made plotting options as quick examples
# to visualise and explore the data. You have less options to modify
# and adjust your plots. We encourage you to explore your own plotting options in
# R (for example using the ggplot2 package) further. Some of these examples use
# the ggplot2 package already - you can customise them further (different colour,
# background, etc.). Plotting in R is very versatile (and could include even things
# like making animations) and it would be too much to go over each separate option.

# Example plot 1: plotting a time series of temperature ("temp")
ggplot(df1) +
  geom_line(aes(date, temp))

# Example plot 2: plotting a profile, split for different dates
ggplot(df2) +
  geom_line(aes(depth, temp)) +
  coord_flip() +
  facet_wrap(~ date)

# Example plot 3: plot a top-view (for the last date in df3)
plot_topview(df = df3[date == max(date)], var = "temp")
# You can see the underlying code by: getAnywhere(plot_topview)

# Example plot 4: plot a transect
# This is a bit more complex and assumes you have a set of coordinates where
# you want to extract data. There is an example of a transect file on the Github repository
the_transect = fread(system.file("extdata", "transect_west_east_northernmost.csv", package="pygetmtools"))
visualise_transect(the_transect, bathy_nc = bathy_nc, depth_name = depth_name)
df4 = read_multiple(output_ncs, var = "temp", transect = the_transect, profile_interval = 1.0)
plot_sideview(df4[date == max(date)], var = "temp")

### Saving a plot as a png
ggsave("example_plot_pygetmtools.png", width = 8, height = 6)
# By default, this saves the last plot you made. You can make this more explicit
# assigning the plot to a variable (e.g. p = ggplot(...) etc.) and then specifying
# ggsave("path/to/file.png", plot = p)

##### More specific and advanced usage #####
# In the next part of this script, you can find some more advanced features and
# uses of the package. 

### Inflow file generation -----
# This creates a netcdf inflow file from a csv. The time series in the csv does
# not need to be continuous.

# Create an example csv file, with two short pulses of a tracer called "Tracer"
df_tracer = data.table(date = as.POSIXct(c("2014-01-01", "2014-04-01", "2014-04-02",
                                           "2014-04-03", "2014-04-04", "2014-07-01",
                                           "2014-07-02", "2014-07-03", "2014-07-04",
                                           "2015-01-01"), tz = "UTC"),
                       flow = c(0,0,1.0,1.0,0,0,1,1,0,0),
                       Tracer_c = c(0,0,100,100,0,0,100,100,0,0))
ggplot(df_tracer) +
  geom_line(aes(date, Tracer_c)) +
  geom_point(aes(date, Tracer_c))
fwrite(df_tracer, "tracer_file.csv")

# Use function to generate a netcdf file that can be read by pygetm's read_from_netcdf function
create_inflow_nc_from_csv("tracer_file.csv", lat = 59.44, lon = 17.43,
                          file_out = "example_tracer_file.nc", timestep = "1 hour",
                          fabmyaml = NULL)

### Bathymetry smoothing -----
# Step 1: read bathymetry as a matrix into R
bathy = read_bathy_as_matrix(bathy_nc, var_name = depth_name)
plot_bathy(bathy) # Add "rev_x = T" and/or "rev_y = T" if your coordinates are provided
                  # in descending order

# Step 2: Smooth and plot repeatedly until you are happy with the result.
# Note that the smooth_bathy_matrix functions has a LOT of options, and the most
# important one is to fix certain coordinates and expand from those ("local" smoothing)
# or just use a moving window (then set maintain_coords to NULL), or a combination.
# By default, you set a maximum value for "rx0". rx0 =  delta-H / (2 * mean(H1,H2)),
# Beckmann & Haidvogel, 1993, doi:10.1175/1520-0485(1993)023<1736:NSOFAA>2.0.CO;2
bathy_s1 = smooth_bathy_matrix(bathy, max_val = 0.15, method = "rx0",
                               maintain_coords = NULL,
                               adjust_factor_global = 0.02)
plot_bathy(bathy_s1, mtrx_ref = bathy, rev_y = T)
plot_bathy(bathy_s1, plot_slope_method = "rx0", rev_y = T)

# Step 3: add the smoothed bathymetry to the netcdf
add_bathy_to_ncdf(bathy_s1, ncdf = bathy_nc, depth_name_new = "bathy_smoothed2",
                  depth_name_orig = depth_name)

### Calculate flow through an interface -----
# Specify the x and y and direction of the flow (x/y). "sign" and "interface_pos" are optional and
# "1" by default
the_interface = list(list(x = 594750, y = 6599850, direction = "y", sign = -1, interface_pos = 1),
                     list(x = 594750, y = 6599850, direction = "x", sign = 1, interface_pos = 1),
                     list(x = 594450, y = 6599850, direction = "y", sign = -1, interface_pos = 1),
                     list(x = 594150, y = 6599250, direction = "x", sign = 1, interface_pos = 1))

# Plot to see if the interface is what you intend it to be ("plotting_window", "arrow_length", and "arrow_tip"
# can be used to modify the plot)
plot_interface(ncdfs = output_ncs, interface = the_interface,
               plotting_window = 5L)

# Calculate the flow through the interface (in m3/s)
df_flow = flux_interface(ncdfs = output_ncs, interface = the_interface)
ggplot(df_flow) +
  geom_line(aes(date, flow))

### Pair observations and simulations -----
# This function finds the closest modelled value to your observations (in terms
# of x, y, z, and time). It does not interpolate.

# Create example csv
df_obs = data.table(date = as.POSIXct(c("2015-04-08 12:00:00", "2015-04-08 12:00:00")),
                    x = c(646548, 646548), y = c(6626576, 6626576), depth = c(0.0, -10.0),
                    temp = c(5.1, 4.8))
fwrite(df_obs, "obs_file.csv")

# Result is returned as a list, with a data.table for each variable requested.
# The list_vars argument is in the format "pygetm_name = obs_file_column_name"
# If the closest simulation is more than max_time_diff away, it will not be used
lst_paired = pair_sim_and_obs(ncs = output_ncs,
                              obs = "obs_file.csv",
                              list_vars = list(temp = "temp"),
                              max_time_diff = lubridate::days(3))
df_temp = lst_paired[["temp"]]
