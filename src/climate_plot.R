################################################################################
#city.w <- get(load(file = '~/Desktop/dvis/data/cities/201508181.RData'))
#res <- climate.plot(city.w, 'Barcelona', '2015')
climate.plot <- function(weather, name, year, variable = 'temperature') {
################################################################################
  # Define variables
  if (variable == 'temperature') {
    name.var <- 'Air Temperature (°C)'
    vars <- c('min_temperaturec', 'max_temperaturec', 'mean_temperaturec')
    optc <- 'D'
  } else if (variable == 'dew') {
    name.var <- 'Dew Point (°C)'
    vars <- c('min_dewpointc', 'dew_pointc', 'meandew_pointc')
    optc <- 'D'
  } else if (variable == 'humidity') {
    name.var <- 'Relative Humidity (%)'
    vars <- c('min_humidity', 'max_humidity', 'mean_humidity')
    optc <- 'D'
  } else if (variable == 'pressure') {
    name.var <- 'Sea Level Pressure (hPa)'
    vars <- c('min_sea level pressurehpa', 'max_sea level pressurehpa',
              'mean_sea level pressurehpa')
    optc <- 'D'
  } else if (variable == 'visibility') {
    name.var <- 'Visibility (Km)'
    vars <- c('min_visibilitykm', 'max_visibilitykm', 'mean_visibilitykm')
    optc <- 'D'
  } else if (variable == 'wind') {
    name.var <- 'Wind Speed (Km/h)'
    vars <- c('mean_wind speedkm/h', 'max_wind speedkm/h', 'max_wind speedkm/h')
    optc <- 'D'
  }

  extremes <- c(min(weather[, vars[1]]), max(weather[, vars[2]]))

  # Set current locale
  clocale <- Sys.getlocale()
  Sys.setlocale('LC_TIME', 'en_US.UTF-8')

  # Generate plot
  out <- ggplot(city.w, aes(date2,
                ymin = get(vars[1]),
                ymax = get(vars[2]),
                color = get(vars[3]))) + 
                geom_linerange(size = 1.3, alpha = 0.75) +
                scale_color_viridis(NULL, option = optc) +
                scale_x_date(labels = date_format('%b'),
                             breaks = date_breaks('month')) + 
                ylim(extremes[1] - 2, extremes[2] + 2) + 
                labs(x = paste(name.var, 'range')) +
                ggtitle(bquote(atop(bold(.(paste(name, 'Weather Radial', year))),
                               atop(.(name.var), '')))) +
                coord_polar() + 
                theme_minimal() +
                theme(legend.position = 'bottom')

  # End
  return(out)
}
