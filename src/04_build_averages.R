################################################################################
# Script: 04_build_averages.R
# source('~/Desktop/bgse/projects/dvis/src/00_start.R')
################################################################################

# Chosen cities
cool.cities <- get(load(file = end.file))
cat('Loaded file:', end.file, '\n')

# Directory where the data is stored
dir <- paste(DATDIR, 'cities/', sep = '')
setwd(dir)
files <- list.files()

# Create aggregates for each city
for (city in 1:nrow(cool.cities)) {
  # Load each year for each city
  cat('City: ', cool.cities[city, 1], '...\n', sep = '')
  city.code <- cool.cities[city, 'WMO']
  city.files <- files[grep(city.code, files)]
  city.files <- city.files[! grepl('avg', city.files)]
  full <- vector(mode = 'list', length(city.files))
  for (i in 1:length(city.files)) {
    year <- get(load(file = city.files[i]))
    year[, 'day'] <- format(year[, 'date2'], '%m-%d')
    full[[i]] <- year
  }
  full <- as.data.frame(rbindlist(full))

  ##############################################################################
  # Mean
  agg <- aggregate(full, by = list(full[, 'day']), mean, na.rm = TRUE)
  agg[, 'date'] <- paste('2015', agg[, 'Group.1'], sep = '-')
  agg[, 'Group.1'] <- NULL

  # New variables
  agg[, 'id'] <- seq(nrow(agg))
  agg[, 'date2'] <- as.Date(agg[, 'date'])
  agg[, 'tmstmp'] <- datetime_to_timestamp(agg[, 'date2'])
  agg[, 'month'] <- month(agg[, 'date2'])
  agg[, 'day'] <- NULL

  # Save results
  city.w <- agg
  file <- paste(dir, 'avg', city.code, '.RData', sep = '')
  save(city.w, file = file); cat('Saved file', file, '\n')

  ##############################################################################
  # Mean
  agg <- aggregate(full[, ! sapply(full, class) %in% c('factor', 'character')],
                   by = list(full[, 'day']), median, na.rm = TRUE)
  agg[, 'date'] <- paste('2015', agg[, 'Group.1'], sep = '-')
  agg[, 'Group.1'] <- NULL

  # New variables
  agg[, 'id'] <- seq(nrow(agg))
  agg[, 'date2'] <- as.Date(agg[, 'date'])
  agg[, 'tmstmp'] <- datetime_to_timestamp(agg[, 'date2'])
  agg[, 'month'] <- month(agg[, 'date2'])
  agg[, 'day'] <- NULL

  # Save results
  city.w <- agg
  file <- paste(dir, 'med', city.code, '.RData', sep = '')
  save(city.w, file = file); cat('Saved file', file, '\n')
}
# END OF SCRIPT
