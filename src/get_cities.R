

library(XML)
library(data.table)
library(gdata)
library(highcharter)
library(ggplot2)
library(viridis)
library(lubridate)

INPDIR <- '~/Desktop/dvis/inp/'
DATDIR <- '~/Desktop/dvis/data/'
TMPDIR <- '~/Desktop/dvis/tmp/'

file <- paste(DATDIR, 'city_list.RData', sep = '')
if (! file.exists(file)) {
  # Download the list of available cities
  destfile <- paste(INPDIR, 'raw_city_list.html', sep = '')
  url <- 'https://www.wunderground.com/about/faq/international_cities.asp'
  download.file(url, destfile = destfile)
  html <- htmlTreeParse(destfile, useInternal = TRUE)
  raw <- xpathSApply(html, '//pre', xmlValue)
  aux <- unlist(strsplit(raw, '\\n'))
  #noms <- unlist(strsplit(aux[2], ' '))
  #noms <- noms[nchar(noms) > 0]
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
    #splitted <- unlist(strsplit(x, '  '))
    #splitted <- splitted[nchar(splitted) > 0]
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

# Cities to show
file <- paste(DATDIR, 'cool_cities.csv', sep = '')
cool <- read.csv(file = file, stringsAsFactors = FALSE); cat('Read file:', file, '\n')
ccities <- as.character(cool[, 1])

# Choose them
new.cols <- c('Zone', 'DisplayName', 'FullCountry')
pm <- pmatch(tolower(ccities), tolower(cities[, 1]))
cool.cities <- cbind.data.frame(cities[pm, ], cool[, 2:4])
colnames(cool.cities)[(ncol(cool.cities) - 2):ncol(cool.cities)] <- new.cols
rownames(cool.cities) <- NULL

# Label error
perth <- which(cool.cities[, 1] == 'Perth')
cool.cities[perth, 2] <- 'WE'
cool.cities[perth, 3] <- 'AU'
cool.cities[perth, 4] <- 'YPPH'
cool.cities[perth, 5] <- -31.93
cool.cities[perth, 6] <- 115.95
cool.cities[perth, 7] <- 20
cool.cities[perth, 8] <- '94610'

# URL bulding blocks
#https://www.wunderground.com/history/airport/KSEA/2015/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2015&req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=&format=1
txt1 <- 'https://www.wunderground.com/history/airport/'
txt2 <- '/'
txt3 <- '/1/1/CustomHistory.html?dayend=31&monthend=12&yearend='
txt4 <- '&req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=&format=1'

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
      good <- FALSE
      while (good == FALSE) {
        # Let the system sleep
        Sys.sleep(jitter(0.7, factor = 10))
        aux <- try(download.file(url = url, destfile = file))
        if (class(aux) != 'try-error') {
          good <- TRUE
        }
      }      
    }

    # Read the raw CSV file
    city.w <- read.csv(file = file)
    if (nrow(city.w) == 0) { next }
    colnames(city.w) <- c('date', 'max_temperaturec', 'mean_temperaturec',
                          'min_temperaturec', 'dew_pointc', 'meandew_pointc',
                          'min_dewpointc', 'max_humidity', 'mean_humidity',
                          'min_humidity', 'max_sea level pressurehpa',
                          'mean_sea level pressurehpa',
                          'min_sea level pressurehpa', 'max_visibilitykm',
                          'mean_visibilitykm', 'min_visibilitykm',
                          'max_wind speedkm/h', 'mean_wind speedkm/h',
                          'max_gust speedkm/h', 'precipitationmm', 'cloudcover',
                          'events', 'winddirdegrees')

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


stop()
clocale <- Sys.getlocale()
Sys.setlocale("LC_TIME", "en_US.UTF-8")

pl <- ggplot(city.w, aes(date2,
               ymin = min_temperaturec,
               ymax = max_temperaturec,
               color = mean_temperaturec)) + 
  geom_linerange(size = 1.3, alpha = 0.75) +
  scale_color_viridis(NULL, option = "D") +
  scale_x_date(labels = date_format("%b"), breaks = date_breaks("month")) + 
  ylim(-10, 35) + 
  labs(x="Air Temperature (°C) range") +
  ggtitle(expression(atop(bold("Lisbon Wather Radial 2015"),
                     atop("Air Temperature (°C)", "")))) +
  # labs(title = "San Francisco Wather Radial",
  #      subtitle = "It would be nice if someone do this with the animation package",
  #      caption = "Other example for ggplot2 vs base #boring but #fun",
  #      x = NULL, y = NULL) +
  coord_polar() + 
  theme_minimal() +
  #theme_jbk() +
  #theme_jbk(base_family = "Arial", plot_title_family = "Arial") + 
  theme(legend.position = "bottom")

png(paste(TMPDIR, 'plot.png'))
plot(pl)
dev.off()


Sys.setlocale(clocale)








