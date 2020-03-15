# Author: Jeff Renz
# Created 3/3/2020 
# 2020 Shiny Challenge

library(RODBC)
library(stringr)
library(httr)

# Virus database connection string
get_database_connection_string <- function() {
  virus_connection_string <- odbcDriverConnect('driver={SQL Server};server=lwjr.database.windows.net;database=HBAP;UID=shiny2020;PWD=c0ronavirus_is_sc@ry!;')
  return(virus_connection_string)
}

# Get Global Stats
get_global_stats <- function() {
  #conn <-get_database_connection_string()
  #global_stats_df <- sqlQuery(conn, "SELECT * FROM [dbo].[GlobalStats];")
  #global_stats <- global_stats_df[c(1,3:1)]
  #close(conn)
  
  #hard code
  global_stats <- list("TotalConfirmed" = 153584, "TotalDeaths" = 5790, "TotalRecovered" = 72587)

  return(global_stats)
  
#print(global_stats$TotalConfirmed)
}

get_virus_stats <- function(p_row_id, p_col_id, p_debug=FALSE) {
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
  
  #values string
  # values <-paste(Area
  #                ,VirusDate
  #                ,Cases
  #                ,Recovered
  #                ,Deaths
  #                , sep=",")
  values <- list("Area" = Area, "VirusDate" = VirusDate, "Cases" = Cases, "Recovered" = Recovered,"Deaths" = Deaths)
  
   
}

insert_virus_stats <- function(p_row_id, p_col_id, p_debug=FALSE) {
  row_virus_data <- get_virus_stats(p_row_id, p_col_id, FALSE)
  p_virus_area = row_virus_data["Area"]
  p_virus_date = row_virus_data["VirusDate"]
  p_cases = row_virus_data["Cases"]
  p_recovered = row_virus_data["Recovered"]
  p_deaths = row_virus_data["Deaths"]


  # SQL Statement to insert data
  sqlStatement <-paste("EXEC [dbo].[prcInfectionDetails]"
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


#row_virus_data <- get_virus_stats(46,285,FALSE)
#print(row_virus_data["Area"])

#print(virus_area)
#print(get_virus_stats(46,285,FALSE))
#insert_virus_stats(46,285,TRUE)
