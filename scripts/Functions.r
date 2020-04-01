# Author: Jeff Renz
# Created 3/3/2020 
# 2020 Shiny Challenge

library(RODBC)
library(odbc)
library(stringr)
library(httr)
library(plyr) 
library(DBI)
library(dplyr)
library(extrafont)

# Load Extra Fonts
#font_import()

# This will show more detailed information about fonts
#fonttable()

#loadfonts()
# If you want to output to .ps files instead of .pdf, use:
# loadfonts(device="postscript")

# Virus database connection string
get_database_connection_string <- function() {
  #RODBC Windows
  #virus_connection_string <- odbcDriverConnect('driver={SQL Server};server=lwjr.database.windows.net;database=HBAP;UID=shiny2020;PWD=c0ronavirus_is_sc@ry!;')
  
  #RODBC Linux
  virus_connection_string <-odbcDriverConnect(connection = "Driver=FreeTDS;TDS_Version=7.2;Server=lwjr.database.windows.net;Port=1433;Database=HBAP;Uid=shiny2020;Pwd=c0ronavirus_is_sc@ry!;Encrypt=yes;TrustServerCertificate=no;Connection Timeout=30;")
  
  #odbc
  #virus_connection_string <- dbConnect(odbc::odbc(), .connection_string = "Driver={SQL Server};server=lwjr.database.windows.net;database=HBAP;UID=shiny2020;PWD=c0ronavirus_is_sc@ry!", timeout = 10)
  return(virus_connection_string)
}

# PDF name for WHO Situation Report
get_situatation_report_pdf <- function() {
  conn <-get_database_connection_string()
  
  #RODBC
  SituatationReportPDF_df <- sqlQuery(conn, "EXEC dbo.selSituationReportName")
  situatation_report_name <- SituatationReportPDF_df$SituatationReportPDF
  close(conn)
  
  #hard code
  #situatation_report_name <-"https://www.who.int/docs/default-source/coronaviruse/situation-reports/20200319-sitrep-59-covid-19.pdf"
  
  return(situatation_report_name)
}


# Data for Country drop down menu
get_countries <- function() {
  conn <-get_database_connection_string()
  
  #RODBC
  countries_df <- sqlQuery(conn, "SELECT DISTINCT CountryOrRegion FROM dbo.Geography WHERE RS LIKE 'https://github.com/CSSEGISandData/COVID-19' ORDER BY CountryOrRegion")
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
# Get data for "By the Numbers" tab
get_current_stats_by_state_province <- function(pCountry) {
  conn <-get_database_connection_string()

  #RODBC
  sql <- paste0("EXEC dbo.selCurrentTotalsForStateProvince @pCountry = '",pCountry,"';")
  current_stats_by_state_province_df <- sqlQuery(conn, sql)
  close(conn)
  #keepcolumns <- c("CountryOrRegion","CityOrStateOrProvince","ConfirmedCases","Recovered","Deaths","CFR")
  #current_stats_by_state_province_df <- subset(current_stats_by_state_province_df, select=keepcolumns)
  #names(current_stats_by_state_province_df) <- c("Country_Region", "State_Province","Cases","Recovered","Deaths","CFR")
  return(current_stats_by_state_province_df)
}

# Get Data for Side Bar Box Plot
get_current_stats_by_country_region <- function() {
  conn <-get_database_connection_string()

  #RODBC
  current_stats_by_country_region_df <- sqlQuery(conn, "EXEC dbo.selInfectionHumdataCurrentTotals;")
  close(conn)
  return(current_stats_by_country_region_df)
}

# Data for Trend Graph
get_trend_data <- function(pCountry,pDaysBack) {
  conn <-get_database_connection_string()
  #RODBC
 
  sql_trend_select <- paste0("EXEC [dbo].[selInfectionHumdataTrendCountry] @pCountryRegion = '",pCountry,"',@pDaysBack = ",pDaysBack,";")
  #sql_trend_select <- paste0("EXEC [dbo].[selInfectionHumdataTrendCountry] @pCountryRegion = '",pCountry,"';")
  trend_df <- sqlQuery(conn, sql_trend_select)
  close(conn)
  return(trend_df)
}

# Data for Current Totals by country  selInfectionHumdataCurrentTotalsForCountry
get_current_totals_for_country <- function(pCountry) {
  conn <-get_database_connection_string()
  
  #RODBC
  sql <- paste0("EXEC [dbo].[selInfectionHumdataCurrentTotalsForCountry] @pCountry = '",pCountry,"';")
  current_totals_for_country_df <- sqlQuery(conn, sql)
  close(conn)
  return(current_totals_for_country_df)
}


# Data for Global Totals
get_global_stats <- function() {
  conn <-get_database_connection_string()
  
  #RODBC
  global_stats_df <- sqlQuery(conn, "EXEC dbo.selInfectionHumdataGlobalTotals;")
  #global_stats <- global_stats_df[c(1,3:1)]
  close(conn)
  return(global_stats_df)
  
#print(global_stats$TotalConfirmed)
}

# gs <- get_global_stats()
# 
# 
# d=Sys.Date()-1#as.Date('2010-01-01')
# format(d, "%m-%d-%Y")



# trend_df <- get_trend_data("US")
# trend_df$ReportDate <- as.Date(trend_df$ReportDate)
# 
# predlm = lm(NewConfirmedCases ~ DaysAgo, data = trend_df)
# predslm = predict(predlm, interval = "confidence")
# 
# # Make Predictions
# pred_today<-predict(predlm, data.frame(DaysAgo = c(0)), interval = "confidence")
# pred_tomorrow<-predict(predlm, data.frame(DaysAgo = c(-1)), interval = "confidence")
# predslm = rbind(predslm,pred_today)
# predslm = rbind(predslm,pred_tomorrow)
# 
# # Add rows for today and tomorrow
# new_values_today <- c("US",as.character(Sys.Date()),0,NA,NA,NA,NA,NA,NA)
# new_values_tomorrow <- c("US",as.character(Sys.Date()+1),-1,NA,NA,NA,NA,NA,NA)
# trend_df = rbind(trend_df,new_values_today)
# trend_df = rbind(trend_df,new_values_tomorrow)
# 
# # Add predictions 
# trend_df = cbind(trend_df, predslm)
# tail(trend_df)
