################################################################################
# Script: 01_city_list.R
# source('~/Desktop/bgse/projects/dvis/src/00_start.R')
################################################################################

# Define the set of available cities to scrap
file <- paste(DATDIR, 'city_list.RData', sep = '')
if (! file.exists(file)) {
  # Download the list of available cities
  destfile <- paste(INPDIR, 'raw_city_list.html', sep = '')
  url <- 'https://www.wunderground.com/about/faq/international_cities.asp'
  download.file(url, destfile = destfile)
  html <- htmlTreeParse(destfile, useInternal = TRUE)
  raw <- xpathSApply(html, '//pre', xmlValue)
  aux <- unlist(strsplit(raw, '\\n'))
  noms <- c('Station', 'State', 'Country', 'ID', 'Lat', 'Lon', 'Elev', 'WMO')

  # Parse the data in a manageable form
  data <- aux[4:length(aux)]
  trimmed <- lapply(1:length(data), function(i) {
    x <- data[i]
    station <- trim(substr(x, 1, 26))
    state <- trim(substr(x, 27, 28))
    country <- trim(substr(x, 29, 31))
    id <- trim(substr(x, 34, 37))
    lat <- trim(substr(x, 38, 49))
    long <- trim(substr(x, 50, 56))
    elev <- trim(substr(x, 58, 63))
    wmo <- trim(substr(x, 64, nchar(x)))
    gathered <- c(station, state, country, id, lat, long, elev, wmo)
    gathered[nchar(gathered) == 0] <- NA
    splitted <- as.character(gathered)
    return(as.data.frame(t(as.data.frame(splitted))))
  })

  # Put it all together
  cities <- as.data.frame(rbindlist(trimmed))
  names(cities) <- noms
  for (col in 1:ncol(cities)) {
    cities[, col] <- as.character(cities[, col])
    if (col %in% 5:7) {
      cities[, col] <- as.numeric(cities[, col])
    }
  }

  # Save results
  save(cities, file = file); cat('Saved file:', file, '\n')
} else {
  cities <- get(load(file = file)); cat('Loaded file:', file, '\n')
}
# END OF SCRIPT
