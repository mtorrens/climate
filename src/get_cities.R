

file <- paste(INPDIR, 'city_list.csv')

if (! file.exists(file)) {
  url <- 'https://www.wunderground.com/about/faq/international_cities.asp'
  download.file(url, destfile = file)  
}



