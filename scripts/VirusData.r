# Author: Jeff Renz
# Created 3/25/2020 
# 2020 Shiny Challenge

library(RODBC)
library(odbc)
library(stringr)
library(httr)
library(DBI)
library(RCurl)

# Run functions.r script to load
rel_path_from_root <- "scripts/Functions.r"
source(rel_path_from_root)

# create list of stats 
insert_virus_stats_daily <- function(p_row_id, p_debug=FALSE) {
  
  # Get GeographyKey
  virus_geographykey <- virus_data_df$Combined_Key[p_row_id]
  virus_geographykey <- paste0("'",virus_geographykey,"'")
  
  if (p_debug) {
    print(paste0("Geography Key: ", virus_geographykey))
  }
  
  # Get City
  virus_city <- virus_data_df$Admin2[p_row_id]
  virus_city <- str_replace_all(virus_city, "\\'","")
  virus_city <- str_replace_all(virus_city, "\\*","")
  virus_city <- paste0("'",virus_city,"'")
  
  if (p_debug) {
    print(paste0("City: ", virus_city))
  }
  
  # Get Province.State
  virus_province_state <- virus_data_df$Province_State[p_row_id]
  virus_province_state <- str_replace_all(virus_province_state, "\\'","")
  virus_province_state <- str_replace_all(virus_province_state, "\\*","")
  virus_province_state <- paste0("'",virus_province_state,"'")
  
  if (p_debug) {
    print(paste0("Province or State: ", virus_province_state))
  }
  
  # Get Country.Region
  virus_country_region <- virus_data_df$Country_Region[p_row_id]
  virus_country_region <- str_replace_all(virus_country_region, "\\'","")
  virus_country_region <- str_replace_all(virus_country_region, "\\*","")
  virus_country_region <- paste0("'",virus_country_region,"'")
  
  if (p_debug) {
    print(paste0("Country or Region: ", virus_country_region))
  }
  
  # Get Lat
  virus_lat <- virus_data_df$Lat[p_row_id]
  if (p_debug) {
    print(paste0("Lat: ", virus_lat))
  } 
  
  # Get Long
  virus_long <- virus_data_df$Long_[p_row_id]
  if (p_debug) {
    print(paste0("Lat: ", virus_long))
  } 
  
  # Get Date
  #format before 3/23
  #virus_date <- substr(virus_data_df$Last_Update[p_row_id],1,7)
  #virus_date <-paste0("'",virus_date,"20'")
  
  #format as of 3/23
  virus_date <- substr(virus_data_df$Last_Update[p_row_id],1,10)
  virus_date <-paste0("'",virus_date,"'")
  if (p_debug) {
    print(paste0("Virus Date: ", virus_date))
  }
  
  #Get Cases
  virus_cases <- toString(virus_data_df$Confirmed[p_row_id])
  if (virus_cases == 'NA') {virus_cases=0}
  if (p_debug) {
    print(paste0("Virus cases: ", virus_cases))
  } 
  
  #Get Deaths
  virus_deaths <- toString(virus_data_df$Deaths[p_row_id])
  if (virus_deaths == 'NA') {virus_deaths=0}
  if (p_debug) {
    print(paste0("Virus deaths: ", virus_deaths))
  }
  
  #Get Recovered
  virus_recovered <- toString(virus_data_df$Recovered[p_row_id])
  if (virus_recovered == 'NA') {virus_recovered=0}
  if (p_debug) {
    print(paste0("Virus recovered: ", virus_recovered))
  }
  
  
  # SQL Statement to insert data
  sqlStatement <-paste("EXEC [dbo].[prcLoadInfectionDetailsHumdata]"
                       , paste("@pGeographyKey = ",virus_geographykey,",",sep="")
                       , paste("@pCity = ",virus_city,",",sep="")
                       , paste("@pProvinceState = ",virus_province_state,",",sep="")
                       , paste("@pCountryRegion = ",virus_country_region,",",sep="")
                       , paste("@pLat = ",virus_lat,",",sep="")
                       , paste("@pLong = ",virus_long,",",sep="")
                       , paste("@pReportDate = ",virus_date,",",sep="")                       
                       , paste("@pConfirmedCases = ",virus_cases,",",sep="")
                       , paste("@pRecovered = ",virus_recovered,",",sep="")
                       , paste("@pDeaths = ",virus_deaths,sep="")
                       , sep=" ")
  if (p_debug) {
    print(paste0("sqlStatement: ", sqlStatement))
  }
  else
  {
    # load connection string
    conn <- get_database_connection_string()

    # Execute SQL
    sql_message <-sqlQuery(conn, sqlStatement)

    if (length(sql_message)==0)
    {
      values_humdata <- list("province_city" = virus_city,"province_state" = virus_province_state, "country_region" = virus_country_region, "virus_lat"=virus_lat,"virus_long"=virus_long, "virus_date" = virus_date, "virus_cases" = virus_cases, "virus_recovered" = virus_recovered,"virus_deaths" = virus_deaths)
      print(paste0("Sucessfully inserted record: ", paste( unlist(values_humdata), collapse='|')))
    }
    else
    {
      print(paste0("Error Message: ", sql_message))
    }

    # Close connection string
    close(conn)
  }
  #values_humdata <- list("province_state" = virus_province_state, "country_region" = virus_country_region, "virus_lat"=virus_lat,"virus_long"=virus_long, "virus_date" = virus_date, "virus_cases" = virus_cases, "virus_recovered" = virus_recovered,"virus_deaths" = virus_deaths)
}

# Virus Data from https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports

# load data
data_load_date=Sys.Date()-1 #yesterday's date is Sys.Date-1
days_back <- 0 # 0 is yesterday
j <- 1
i <- 0
while (i<=days_back)
{
  # Get daily data 
  print(data_load_date)
  virus_date <- format(data_load_date, "%m-%d-%Y")# Example format "03-23-2020"
  virus_data_url <-paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",virus_date,".csv")
  print(virus_data_url)
  virus_data_df <-read.csv(text=getURL(virus_data_url), skip=0, header=T)
  
  virus_data_df <- subset(virus_data_df,virus_data_df$Confirmed>0|virus_data_df$Deaths>0|virus_data_df$Recovered>0)
  row_count_virus_data <- nrow(virus_data_df)  
  
  while (j<=row_count_virus_data)
    {
    print(paste0("Row:", j," ReportDate:" ,virus_data_df$Last_Update[j]," GeographyKey:",virus_data_df$Combined_Key[j]))
    insert_virus_stats_daily(j,FALSE)
    # Get next row
    j <- j+1
    }
  # previous day
  data_load_date <- data_load_date -1
  i <- i+1
}

# Test Examples
#insert_virus_stats_daily(1,FALSE)