library(RODBC)
conn <- get_database_connection_string()
virusdata <- sqlQuery(conn, "SELECT * FROM dbo.InfectionDetails;")

#Data Frame Structure
str(virusdata)

#print virus Data
virusdata

global_stats_df <- sqlQuery(conn, "SELECT * FROM [dbo].[GlobalStats];")
global_stats <- global_stats_df[c(1,3:1)]
print(global_stats$TotalConfirmed)

