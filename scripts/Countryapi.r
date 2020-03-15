#install.packages("httr")
library(httr)
library(jsonlite)
country <- "china"
base_url <-"http://restcountries.eu/rest/v2/name/"

#r <- GET(paste0(base_url,country))
r <- GET("http://restcountries.eu/rest/v2/name/china")
rest_status <-http_status(r)
rest_status$category
http_type(r)

json <- content(r, "text")
toJSON(fromJSON(json))

