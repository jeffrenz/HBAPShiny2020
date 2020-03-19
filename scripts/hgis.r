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

# Get Data From hgis web site
virus_raw_data <- read.csv(url("http://hgis.uw.edu/virus/assets/virus.csv"))
rcount <- nrow(virus_raw_data)
ccount <- ncol(virus_raw_data)

# get list of stats 
get_virus_stats_hgis <- function(p_row_id, p_col_id, p_debug=FALSE) {
  # Get Area
  virus_area <- names(virus_raw_data)[p_col_id]
  if (p_debug) {
    print(paste0("Area: ", virus_area))
  }
  
  # Get Date
  virus_date <- toString(virus_raw_data[p_row_id,1])
  if (p_debug) {
    print(paste0("Virus Date: ", virus_date))
  }
  
  #Get Virus stats
  # Example format "200-0-0-1"
  virus_stats <- toString(virus_raw_data[p_row_id,p_col_id])
  if (p_debug) {
    print(paste0("Virus Stats: ", virus_stats))
  }
  
  # parse cases, recovered, deaths
  if (virus_stats != 'NA')
  {
    s <-strsplit(virus_stats, "-")[[1]]
    Cases <- if(is.na(s[1])){0} else {s[1]}
    Recovered <-if(is.na(s[3])){0} else {s[3]}
    Deaths <-if(is.na(s[4])){0} else {s[4]}
  }
  else
  {
    Cases <- 0
    Recovered <- 0
    Deaths <- 0
  }
  
  if (p_debug) {
    print(paste0("Cases: ", Cases))
    print(paste0("Recovered: ", Recovered))
    print(paste0("Deaths: ", Deaths))
  }
  Area <- paste0("'", virus_area, "'") 
  VirusDate <- paste0("'", virus_date, "'") 
  
  values <- list("Area" = Area, "VirusDate" = VirusDate, "Cases" = Cases, "Recovered" = Recovered,"Deaths" = Deaths)
}

# logic to insert into database
insert_virus_stats_hgis <- function(p_row_id, p_col_id, p_debug=FALSE) {
  row_virus_data <- get_virus_stats_hgis(p_row_id, p_col_id, FALSE)
  p_virus_area = row_virus_data["Area"]
  p_virus_date = row_virus_data["VirusDate"]
  p_cases = row_virus_data["Cases"]
  p_recovered = row_virus_data["Recovered"]
  p_deaths = row_virus_data["Deaths"]
  
  
  # SQL Statement to insert data
  sqlStatement <-paste("EXEC [dbo].[prcLoadInfectionDetailsHGIS]"
                       , paste("@pArea = ",p_virus_area,",",sep="")
                       , paste("@pReportDate = ",p_virus_date,",",sep="")
                       , paste("@pConfirmedCases = ",p_cases,",",sep="")
                       , paste("@pRecovered = ",p_recovered,",",sep="")
                       , paste("@pDeaths = ",p_deaths,sep="")
                       , sep=" ")
  
  if (p_debug) {
    print(paste0("SQL Statement: ", sqlStatement))
  } 
  else
  {
    # load connection string
    conn <- get_database_connection_string()
    
    # Execute SQL
    sql_message <-sqlQuery(conn, sqlStatement)
    
    if (length(sql_message)==0)
    {
      print(paste0("Sucessfully inserted record: ", paste( unlist(row_virus_data), collapse='|')))
    }
    else
    {
      print(paste0("Error Message: ", sql_message))
    }
    
    # Close connection string
    close(conn)
  }
}

# loop through data and insert into database
days_back <- 0 # 0 is today
i <-rcount
j <- 2
#while (i>=1)
while (i>=rcount-days_back )
{
  while (j<=ccount)
  {
    print(paste0("Row:", i," Column:" ,j," Value:",virus_raw_data[i,j]))
    insert_virus_stats_hgis(i,j,FALSE)
    #print(get_virus_stats(i,j,FALSE))
    j <- j+1
  }
  j <-2
  i <-i-1
}

#insert_virus_stats(49,2,TRUE)
#insert_virus_stats(8,3,TRUE)