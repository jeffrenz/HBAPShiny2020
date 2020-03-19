# Author: Jeff Renz
# Created 3/3/2020 
# 2020 Shiny Challenge

library(RODBC)
library(odbc)
library(stringr)
library(httr)

library(DBI)

# Run functions.r script to load
rel_path_from_root <- "scripts/Functions.r"
source(rel_path_from_root)

# Get Data From https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases
virus_humdata_confirmed <- read.csv(url("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_19-covid-Confirmed.csv&filename=time_series_2019-ncov-Confirmed.csv"))
virus_humdata_death <- read.csv(url("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_19-covid-Deaths.csv&filename=time_series_2019-ncov-Deaths.csv"))
virus_humdata_recovered <- read.csv(url("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_19-covid-Recovered.csv&filename=time_series_2019-ncov-Recovered.csv"))

row_count_humdata <- nrow(virus_humdata_confirmed)
col_count_humdata <- ncol(virus_humdata_confirmed)

# create list of stats 
insert_virus_stats_humdata <- function(p_row_id, p_col_id, p_debug=FALSE) {
  
  # Get Province.State
  virus_province_state <- virus_humdata_confirmed[p_row_id,1]
  virus_province_state <- str_replace_all(virus_province_state, "\\'","")
  virus_province_state <- str_replace_all(virus_province_state, "\\*","")
  virus_province_state <- paste0("'",virus_province_state,"'")
  
  if (p_debug) {
    print(paste0("Province or State: ", virus_province_state))
  }

  # Get Country.Region
  virus_country_region <- virus_humdata_confirmed[p_row_id,2]
  virus_country_region <- str_replace_all(virus_country_region, "\\'","")
  virus_country_region <- str_replace_all(virus_country_region, "\\*","")
  virus_country_region <- paste0("'",virus_country_region,"'")
  
  if (p_debug) {
    print(paste0("Country or Region: ", virus_country_region))
  }
  
  # Get Lat
  virus_lat <- virus_humdata_confirmed[p_row_id,3]
  if (p_debug) {
    print(paste0("Lat: ", virus_lat))
  } 

  # Get Long
  virus_long <- virus_humdata_confirmed[p_row_id,4]
  if (p_debug) {
    print(paste0("Lat: ", virus_long))
  } 
  
  # Get Date
  virus_date <- names(virus_humdata_confirmed)[p_col_id]
  virus_date <- str_replace(virus_date, "X","")
  virus_date <- str_replace_all(virus_date, "\\.","-")
  virus_date <-paste0("'",virus_date,"20'")
  
  if (p_debug) {
    print(paste0("Virus Date: ", virus_date))
  }
  
  #Get Cases
  virus_cases <- toString(virus_humdata_confirmed[p_row_id,p_col_id])
  if (p_debug) {
    print(paste0("Virus cases: ", virus_cases))
  }

  #Get Deaths
  virus_deaths <- toString(virus_humdata_death[p_row_id,p_col_id])
  if (p_debug) {
    print(paste0("Virus deaths: ", virus_deaths))
  }
  
  #Get Recovered
  virus_recovered <- toString(virus_humdata_recovered[p_row_id,p_col_id])
  if (p_debug) {
    print(paste0("Virus recovered: ", virus_recovered))
  }

  # SQL Statement to insert data
  sqlStatement <-paste("EXEC [dbo].[prcLoadInfectionDetailsHumdata]"
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
      values_humdata <- list("province_state" = virus_province_state, "country_region" = virus_country_region, "virus_lat"=virus_lat,"virus_long"=virus_long, "virus_date" = virus_date, "virus_cases" = virus_cases, "virus_recovered" = virus_recovered,"virus_deaths" = virus_deaths)
      print(paste0("Sucessfully inserted record: ", paste( unlist(values_humdata), collapse='|')))
    }
    else
    {
      print(paste0("Error Message: ", sql_message))
    }
    
    # Close connection string
    close(conn)
  }  
  values_humdata <- list("province_state" = virus_province_state, "country_region" = virus_country_region, "virus_lat"=virus_lat,"virus_long"=virus_long, "virus_date" = virus_date, "virus_cases" = virus_cases, "virus_recovered" = virus_recovered,"virus_deaths" = virus_deaths)
}

# Test Examples
insert_virus_stats_humdata(1,18,TRUE)
#insert_virus_stats_humdata(237,59,TRUE) #has special character ' in string
#insert_virus_stats_humdata(191,59,TRUE) #has special character * in string


# loop through data and insert into HBAP database
# i Columns that represent the date
# j rows that represent the area that is effected

days_back <- 0 # 0 is today
#i <-5 # start of historic data
#i <-col_count_humdata - days_back
j <- 1

while (i<=col_count_humdata-days_back )
{
  while (j<=row_count_humdata)
  {
    print(paste0("Row:", j," Column:" ,i," Cases:",virus_humdata_confirmed[j,i]))
    insert_virus_stats_humdata(j,i,FALSE)
    # Get next row
    j <- j+1
  }
  j <-1
  i <-i+1
}



