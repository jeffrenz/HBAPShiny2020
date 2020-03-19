# Author: Jeff Renz
# Created 3/3/2020 
# 2020 Shiny Challenge

library(RODBC)
library(odbc)
library(stringr)
library(httr)
library(plyr) 
library(DBI)

# World Health Organization Situation Report
situatation_report_pdf <-"https://www.who.int/docs/default-source/coronaviruse/situation-reports/20200318-sitrep-58-covid-19.pdf"

# Virus database connection string
get_database_connection_string <- function() {
  #RODBC Windows
  virus_connection_string <- odbcDriverConnect('driver={SQL Server};server=lwjr.database.windows.net;database=HBAP;UID=shiny2020;PWD=c0ronavirus_is_sc@ry!;')
  
  #RODBC Linux
  #virus_connection_string <-odbcDriverConnect(connection = "Driver=FreeTDS;TDS_Version=7.2;Server=lwjr.database.windows.net;Port=1433;Database=HBAP;Uid=shiny2020;Pwd=c0ronavirus_is_sc@ry!;Encrypt=yes;TrustServerCertificate=no;Connection Timeout=30;")
  
  #odbc
  #virus_connection_string <- dbConnect(odbc::odbc(), .connection_string = "Driver={SQL Server};server=lwjr.database.windows.net;database=HBAP;UID=shiny2020;PWD=c0ronavirus_is_sc@ry!", timeout = 10)
  return(virus_connection_string)
}

# Data for Country drop down menu
get_countries <- function() {
  conn <-get_database_connection_string()
  
  #RODBC
  countries_df <- sqlQuery(conn, "SELECT DISTINCT CountryOrRegion FROM dbo.Geography")
  countries <- countries_df$CountryOrRegion
  close(conn)

  #hard code
  #countries <- c("China","Hong Kong","Thailand","South Korea","Japan","Malaysia","Thailand","US","Australia")
  
  return(countries)
}
  
# Data for Map
get_map_stats <- function() {
  conn <-get_database_connection_string()
  
  #RODBC
  map_stats_df <- sqlQuery(conn, "EXEC dbo.selInfectionMapCountry;")
  coordinates(map_stats_df) <- ~Long+Lat
  close(conn)
  
  return(map_stats_df)
}

# Current Totals by Country
get_current_stats_by_country <- function() {
  conn <-get_database_connection_string()

  #RODBC
  current_stats_by_country_df <- sqlQuery(conn, "EXEC dbo.selInfectionMapCountry;")
  close(conn)
  
  keepcolumns <- c("CountryOrRegion", "CityOrStateOrProvince","ConfirmedCases","Recovered","Deaths")
  current_stats_by_country_df <-subset(current_stats_by_country_df, select = keepcolumns)
  current_stats_by_country_df <-rename(current_stats_by_country_df,c("CityOrStateOrProvince" = "State_Province", "CountryOrRegion" = "Country_Region", "ConfirmedCases" = "Cases"))
  return(current_stats_by_country_df)
}

# Data for Trend Graph
get_trend_data <- function(pCountry) {
  conn <-get_database_connection_string()
  
  #RODBC
  sql_trend_select <- paste0("EXEC [dbo].[selInfectionHumdataTrendCountry] @pCountryRegion = '",pCountry,"';")
  trend_df <- sqlQuery(conn, sql_trend_select)
  close(conn)
  return(trend_df)
}

# Data for Global Totals
get_global_stats <- function() {
  conn <-get_database_connection_string()
  
  #RODBC
  global_stats_df <- sqlQuery(conn, "SELECT * FROM [dbo].[GlobalStats];")
  global_stats <- global_stats_df[c(1,3:1)]
  close(conn)
  return(global_stats)
  
#print(global_stats$TotalConfirmed)
}

# Get Data
#map_stats <- get_map_stats()
#coordinates(map_stats) <- ~Long+Lat
#trend_df <- get_trend_data('us')
#cs <- get_current_stats_by_country()
#head(cs)