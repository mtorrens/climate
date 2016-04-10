################################################################################
# Script: 00_start.R
# source('~/Desktop/bgse/projects/dvis/src/00_start.R')
################################################################################

# Initialize project
cat('Initializing...
* Project: Data Visualization
* Author: (c) Miquel Torrens
* Date: April 2016
* Packages: XML, data.table, gdata, highcharter, ggplot2,
            viridis, lubridate, scales, shiny\n')

# Package dependencies
library(XML)
library(data.table)
library(gdata)
library(highcharter)
library(ggplot2)
library(viridis)
library(lubridate)
library(scales)
library(shiny)

# Load plotting function
source('src/climate_plot.R')

# Load necessary data
cool.cities <- get(load(file = 'cities/cool_cities.RData'))

# Finish
cat('Done!\n')
# END OF SCRIPT
