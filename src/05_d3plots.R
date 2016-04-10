################################################################################
# Script: d3plots.R
source('~/Desktop/bgse/projects/dvis/src/00_start.R')
################################################################################

# Set
setwd(OUTDIR)
files <- list.files()
files <- files[grep('.csv', files)]
#files <- files[! grepl('nyc', files)]
#files <- files[! grepl('mexico', files)]
files <- files[! grepl('2', files)]

# To enhance the size of precipitation
for (file in files) {
  new.file <- paste(strsplit(file, '\\.')[[1]][1], '2.csv', sep = '')
  data <- read.csv(file = file, stringsAsFactors = FALSE)
  data[, 'Precipitationmm'] <- round(5 * as.numeric(data[, 'Precipitationmm']), 0)
  colnames(data) <- gsub('\\.', ' ', colnames(data))
  write.csv(data, file = new.file, row.names = FALSE, quote = FALSE, na = '')
}
# END OF SCRIPT
