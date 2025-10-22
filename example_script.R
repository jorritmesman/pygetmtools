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
