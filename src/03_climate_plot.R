################################################################################
# Script: 03_climate_plot.R
# source('~/Desktop/bgse/projects/dvis/src/00_start.R')
################################################################################

################################################################################
climate.plot <- function(weather, name, year, variable = 'Temperature') {
################################################################################
  # Define variables
  if (variable == 'Temperature') {
    name.var <- 'Air Temperature (°C)'
    vars <- c('min_temperaturec', 'max_temperaturec', 'mean_temperaturec')
    optc <- 'D'
  } else if (variable == 'Dew Point') {
    name.var <- 'Dew Point (°C)'
    vars <- c('min_dewpointc', 'dew_pointc', 'meandew_pointc')
    optc <- 'D'
  } else if (variable == 'Relative Humidity') {
    name.var <- 'Relative Humidity (%)'
    vars <- c('min_humidity', 'max_humidity', 'mean_humidity')
    optc <- 'D'
  } else if (variable == 'Sea Level Pressure') {
    name.var <- 'Sea Level Pressure (hPa)'
    vars <- c('min_sea level pressurehpa', 'max_sea level pressurehpa',
              'mean_sea level pressurehpa')
    optc <- 'D'
  } else if (variable == 'Visibility') {
    name.var <- 'Visibility (Km)'
    vars <- c('min_visibilitykm', 'max_visibilitykm', 'mean_visibilitykm')
    optc <- 'D'
  } else if (variable == 'Wind Speed') {
    name.var <- 'Wind Speed (Km/h)'
    vars <- c('mean_wind speedkm/h', 'max_wind speedkm/h', 'max_wind speedkm/h')
    optc <- 'D'
  } else if (variable == 'Precipitation') {
    name.var <- 'Precipitation (mm)'
    vars <- c('precipitationmm', 'precipitationmm', 'precipitationmm')
    optc <- 'D'
  } else if (variable == 'Cloud coverage') {
    name.var <- 'Cloud coverage (scaled 1 to 6)'
    vars <- c('cloudcover', 'cloudcover', 'cloudcover')
    optc <- 'D'
  }  

  # Add some variablity for static variables
  if (vars[1] == vars[2]) {
    new.var1 <- paste('min', vars[1], sep = '_')
    new.var2 <- paste('max', vars[2], sep = '_')
    weather[, new.var1] <- weather[, vars[3]] - 1
    weather[, new.var2] <- weather[, vars[3]] + 1
    vars <- c(new.var1, new.var2, vars[3])
  }

  # Ends of the variable
  extremes <- c(min(weather[, vars[1]]), max(weather[, vars[2]]))

  # Set current locale
  clocale <- Sys.getlocale()
  Sys.setlocale('LC_TIME', 'en_US.UTF-8')

  # Generate plot
  out <- ggplot(weather, aes(date2,
                ymin = get(vars[1]),
                ymax = get(vars[2]),
                color = get(vars[3]))) + 
                geom_linerange(size = 1.3, alpha = 0.75) +
                scale_color_viridis(NULL, option = optc) +
                scale_x_date(labels = date_format('%b'),
                             breaks = date_breaks('month')) + 
                ylim(extremes[1] - 2, extremes[2] + 2) + 
                labs(x = '') +
                #labs(x = paste(name.var, 'range')) +
                ggtitle(bquote(atop(bold(.(paste(name, 'Weather Radial', year))),
                               atop(.(name.var), '')))) +
                coord_polar() + 
                theme_minimal() +
                theme(legend.position = 'right')

  # End
  return(out)
}
# END OF SCRIPT
