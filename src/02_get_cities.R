################################################################################
# Script: 02_get_cities.R
# source('~/Desktop/bgse/projects/dvis/src/00_start.R')
################################################################################
force <- FALSE     # Force processing of the cities
download <- FALSE  # Perform the downloads (for unsaved cities)
################################################################################

# Cities to show
end.file <- paste(DATDIR, 'cool_cities.RData', sep = '')
if (! file.exists(end.file) | force == TRUE) {
  file <- paste(DATDIR, 'cool_cities.csv', sep = '')
  cool <- read.csv(file = file, stringsAsFactors = FALSE)
  cat('Read file:', file, '\n')
  ccities <- as.character(cool[, 1])
  for (col in 1:ncol(cool)) {
    cool[, col] <- gdata::trim(cool[, col])
  }

  # Choose them
  new.cols <- c('Zone', 'DisplayName', 'FullCountry')
  pm <- pmatch(tolower(ccities), tolower(cities[, 1]))
  cool.cities <- cbind.data.frame(cities[pm, ], cool[, 2:4])
  colnames(cool.cities)[(ncol(cool.cities) - 2):ncol(cool.cities)] <- new.cols
  rownames(cool.cities) <- NULL

  # Some labelling errors
  perth <- which(cool.cities[, 1] == 'Perth')
  cool.cities[perth, 2] <- 'WE'
  cool.cities[perth, 3] <- 'AU'
  cool.cities[perth, 4] <- 'YPPH'
  cool.cities[perth, 5] <- -31.93
  cool.cities[perth, 6] <- 115.95
  cool.cities[perth, 7] <- 20
  cool.cities[perth, 8] <- '94610'
  sydney <- which(cool.cities[, 1] == 'Sydney')
  cool.cities[sydney, 2] <- 'NW'
  cool.cities[sydney, 3] <- 'AU'
  cool.cities[sydney, 4] <- 'YSSY'
  cool.cities[sydney, 5] <- -33.95
  cool.cities[sydney, 6] <- 151.18
  cool.cities[sydney, 7] <- 3
  cool.cities[sydney, 8] <- '94767'
  brasilia <- which(cool.cities[, 1] == 'Brasilia')
  cool.cities[brasilia, 2] <- ''
  cool.cities[brasilia, 3] <- 'BZ'
  cool.cities[brasilia, 4] <- 'SBBR'
  cool.cities[brasilia, 5] <- -33.95
  cool.cities[brasilia, 6] <- 151.18
  cool.cities[brasilia, 7] <- 1061
  cool.cities[brasilia, 8] <- '83378'
  santiago <- which(cool.cities[, 1] == 'Santiago')
  cool.cities[santiago, 2] <- ''
  cool.cities[santiago, 3] <- 'CH'
  cool.cities[santiago, 4] <- 'SCEL'
  cool.cities[santiago, 5] <- -33.38
  cool.cities[santiago, 6] <- -70.78
  cool.cities[santiago, 7] <- 476
  cool.cities[santiago, 8] <- '85574'
  alexandria <- which(cool.cities[, 1] == 'Alexandria')
  cool.cities[alexandria, 2] <- ''
  cool.cities[alexandria, 3] <- 'EG'
  cool.cities[alexandria, 4] <- 'HEAX'
  cool.cities[alexandria, 5] <- 31.20
  cool.cities[alexandria, 6] <- 29.95
  cool.cities[alexandria, 7] <- 7
  cool.cities[alexandria, 8] <- '62318'
  novosibirsk <- which(cool.cities[, 1] == 'Novosibirsk')
  cool.cities[novosibirsk, 4] <- 'UNNT'
  alert <- which(cool.cities[, 1] == 'Alert')
  cool.cities[alert, 4] <- 'CYLT'
  tbilisi <- which(cool.cities[, 1] == 'Tbilisi')
  cool.cities[tbilisi, 4] <- 'UGTB'
  cool.cities[tbilisi, 8] <- '37545'
  borabora <- which(cool.cities[, 1] == 'Bora-Bora')
  cool.cities[borabora, 4] <- 'NTTB'
  cool.cities[borabora, 8] <- '91938'

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
#txt4 <- '&req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo='
txt4 <- '&req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=&format=1'
#txt5 <- '&format=1'

# Final column names
cols <- c('date', 'max_temperaturec', 'mean_temperaturec', 'min_temperaturec',
          'dew_pointc', 'meandew_pointc', 'min_dewpointc', 'max_humidity',
          'mean_humidity', 'min_humidity', 'max_sea level pressurehpa',
          'mean_sea level pressurehpa', 'min_sea level pressurehpa',
          'max_visibilitykm', 'mean_visibilitykm', 'min_visibilitykm',
          'max_wind speedkm/h', 'mean_wind speedkm/h', 'max_gust speedkm/h',
          'precipitationmm', 'cloudcover', 'events', 'winddirdegrees')

# Scrap website
if (download == TRUE) {
  years <- as.character(2015:2000)
  for (year in years) {
    for (city in 1:nrow(cool.cities)) {
      cat('Year:', year, 'City:', cool.cities[city, 1], '\n')

      # Download
      city.code <- cool.cities[city, 'WMO']
      city.init <- cool.cities[city, 'ID']
      url <- paste(txt1, city.init, txt2, year, txt3, year, txt4, sep = '')
      #url <- paste(txt1, city.init, txt2, year, txt3, year, txt4, city.code,
      #             txt5, sep = '')
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
}
# END OF SCRIPT
