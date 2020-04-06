library("stringdist")

sapply(names(COVID19$deaths), match.countries, country.list = x$Country)

match.countries <- function(country, country.list){
  if (country %in% country.list){
    return(c(country, country))
  } else {
    n <- which.min(stringdist(country, country.list))
    return(c(country, country.list[n]))
  }
}