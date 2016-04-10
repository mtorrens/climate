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

# Working directory
PATH <- '~/Desktop/bgse/projects/dvis/'

# Other directories
SRCDIR <- paste(PATH, 'src/', sep = '')
INPDIR <- paste(PATH, 'input/', sep = '')
DATDIR <- paste(PATH, 'data/', sep = '')
TMPDIR <- paste(PATH, 'temp/', sep = '')
OUTDIR <- paste(PATH, 'output/', sep = '')
APPDIR <- paste(PATH, 'app/', sep = '')

# Package dependencies
library(XML)
library(data.table)
library(gdata)
library(highcharter)
library(ggplot2)
library(viridis)
library(lubridate)
library(scales)
library(shiny)

# Source relevant files
source(paste(SRCDIR, '01_city_list.R', sep = ''))
source(paste(SRCDIR, '03_climate_plot.R', sep = ''))
source(paste(SRCDIR, '02_get_cities.R', sep = ''))

# Finish
cat('Done!\n')
# END OF SCRIPT
