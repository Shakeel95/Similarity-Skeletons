library(docstring) # EZ documentation
library(htmltab) # extract html tables
library(BatchGetSymbols) # stock prices

SnP500.info <- htmltab("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies",1)

countries.info <- htmltab("https://meta.wikimedia.org/wiki/List_of_countries_by_regional_classification",1)
names(countries.info)[3] <- "Development"

countries.population <- htmltab("https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population",1)[,2:3]
names(countries.population) <- c("Country","Population")
countries.population$Country <- substring(countries.population$Country,2)
countries.population$Population <- as.numeric(gsub(",","",countries.population$Population[1]))
countries.population$Population <- as.numeric(countries.population$Population)

SnP500.data <- function(date.from, date.to, quiet = FALSE){
  #'SnP500 data 
  #'
  #'Panel of stock prices (market open price) for SnP500 constituents
  #'
  #'@param date.from 
  #'@param date.to 
  #'@param quiet bool, show BatchGetSymbols otput
  
  if (!(inherits(date.from,"Date"))) stop("date.from should be of class Date!")
  if (!(inherits(date.to,"Date"))) stop("date.to should be of class Date")
  
  tickers <- SnP500.info$Symbol
  batch.tickers <- BatchGetSymbols(tickers = tickers, 
                                   first.date = date.from,
                                   last.date = date.to, 
                                   cache.folder = file.path(tempdir(), 'BGS_Cache'),
                                   be.quiet = quiet)
  
  df <- batch.tickers$df.tickers[,c("price.open","ref.date","ticker")]
  tickers <- unique(df$ticker)
  df <- reshape(df, idvar = "ref.date", timevar = "ticker", direction = "wide")
  names(df) <- c("ref.date",tickers)
  rownames(df) <- df$ref.date
  
  return(subset(df, select = -ref.date))
}

COVID19.global <- function(){
  require(docstring)
  #' COVID-19 deaths by country
  #' 
  #' Reads data from Johns Hopkins GitHub repo. Returns T x p panel. 
  
  url.deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
  df.deaths <- subset(read.csv(url.deaths), select =  -c(Province.State, Lat, Long))
  countries.deaths <- unique(df.deaths$Country.Region)
  
  df.deaths <- sapply(countries.deaths, function(i) colSums(df.deaths[df.deaths$Country.Region == i,-1]))
  df.deaths <- data.frame(df.deaths)
  names(df.deaths) <- countries.deaths
  df.deaths <- df.deaths[,-which(apply(df.deaths, 2, function(i) (min(i) == max(i))) == TRUE)]
  
  df.confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  df.confirmed <- subset(read.csv(df.confirmed), select =  -c(Province.State, Lat, Long))
  countries.confirmed <- unique(df.confirmed$Country.Region)
  
  df.confirmed <- sapply(countries.confirmed, function(i) colSums(df.confirmed[df.confirmed$Country.Region == i,-1]))
  df.confirmed <- data.frame(df.confirmed)
  names(df.confirmed) <- countries.confirmed

  df.recovered <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
  df.recovered <- subset(read.csv(df.recovered), select =  -c(Province.State, Lat, Long))
  countries.recovered <- unique(df.recovered$Country.Region)
  
  df.recovered <- sapply(countries.recovered, function(i) colSums(df.recovered[df.recovered$Country.Region == i,-1]))
  df.recovered <- data.frame(df.recovered)
  names(df.recovered) <- countries.recovered
  df.recovered <- df.recovered[,-which(apply(df.recovered, 2, function(i) (min(i) == max(i))) == TRUE)]
  
  return(list(deaths = df.deaths, 
              confirmed.cases = df.confirmed, 
              recovered = df.recovered))
}