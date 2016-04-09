#source('~/Desktop/dvis/src/get_cities.R')
force <- FALSE

source('~/Desktop/dvis/src/start.R')
source(paste(SRCDIR, 'climate_plot.R', sep = ''))
source(paste(SRCDIR, 'city_list.R', sep = ''))

# Cities to show
end.file <- paste(DATDIR, 'cool_cities.R', sep = '')
if (! file.exists(end.file) | force == TRUE) {
  file <- paste(DATDIR, 'cool_cities.csv', sep = '')
  cool <- read.csv(file = file, stringsAsFactors = FALSE)
  cat('Read file:', file, '\n')
  ccities <- as.character(cool[, 1])

  # Choose them
  new.cols <- c('Zone', 'DisplayName', 'FullCountry')
  pm <- pmatch(tolower(ccities), tolower(cities[, 1]))
  cool.cities <- cbind.data.frame(cities[pm, ], cool[, 2:4])
  colnames(cool.cities)[(ncol(cool.cities) - 2):ncol(cool.cities)] <- new.cols
  rownames(cool.cities) <- NULL

  # Label error (Duplicated)
  perth <- which(cool.cities[, 1] == 'Perth')
  cool.cities[perth, 2] <- 'WE'
  cool.cities[perth, 3] <- 'AU'
  cool.cities[perth, 4] <- 'YPPH'
  cool.cities[perth, 5] <- -31.93
  cool.cities[perth, 6] <- 115.95
  cool.cities[perth, 7] <- 20
  cool.cities[perth, 8] <- '94610'

  # Save the list
  save(cool.cities, file = end.file); cat('Saved file:', end.file, '\n')
} else {
  cool.cities <- get(load(file = end.file))
  cat('Loaded file:', end.file, '\n')
}

# URL bulding blocks
txt1 <- 'https://www.wunderground.com/history/airport/'
txt2 <- '/'
txt3 <- '/1/1/CustomHistory.html?dayend=31&monthend=12&yearend='
txt4 <- '&req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=&format=1'

# Final column names
cols <- c('date', 'max_temperaturec', 'mean_temperaturec', 'min_temperaturec',
          'dew_pointc', 'meandew_pointc', 'min_dewpointc', 'max_humidity',
          'mean_humidity', 'min_humidity', 'max_sea level pressurehpa',
          'mean_sea level pressurehpa', 'min_sea level pressurehpa',
          'max_visibilitykm', 'mean_visibilitykm', 'min_visibilitykm',
          'max_wind speedkm/h', 'mean_wind speedkm/h', 'max_gust speedkm/h',
          'precipitationmm', 'cloudcover', 'events', 'winddirdegrees')

# Scrap website
years <- as.character(2015:2000)
for (year in years) {
  for (city in 1:nrow(cool.cities)) {
    cat('Year:', year, 'City:', cool.cities[city, 1], '\n')

    # Download
    city.code <- cool.cities[city, 'WMO']
    city.init <- cool.cities[city, 'ID']
    url <- paste(txt1, city.init, txt2, year, txt3, year, txt4, sep = '')
    file <- paste(INPDIR, 'dwl/', year, city.code, '.csv', sep = '')
    if (! file.exists(file)) {
      done <- FALSE
      while (done == FALSE) {
        # Let the system sleep
        Sys.sleep(jitter(0.7, factor = 10))
        aux <- try(download.file(url = url, destfile = file))
        if (class(aux) != 'try-error') {
          done <- TRUE
        }
      }      
    }

    # Read the raw CSV file
    city.w <- read.csv(file = file)
    if (nrow(city.w) == 0) { next }
    colnames(city.w) <- cols

    # New variables
    city.w[, 'id'] <- seq(nrow(city.w))
    city.w[, 'date2'] <- as.Date(ymd(city.w[, 'date']))
    city.w[, 'tmstmp'] <- datetime_to_timestamp(city.w[, 'date2'])
    city.w[, 'month'] <- month(ymd(city.w[, 'date']))
    city.w[, 'winddirdegrees'] <- gsub('<br />', '', city.w[, 'winddirdegrees'])

    # Save results
    file <- paste(DATDIR, 'cities/', year, city.code, '.RData', sep = '')
    save(city.w, file = file); cat('Saved file:', file, '\n')
  }
}

