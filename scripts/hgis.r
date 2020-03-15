# Run functions.r script to load
rel_path_from_root <- "scripts/Functions.r"
source(rel_path_from_root)

# Get Data From hgis web site
virus_raw_data <- read.csv(url("http://hgis.uw.edu/virus/assets/virus.csv"))
rcount <- nrow(virus_raw_data)
ccount <- ncol(virus_raw_data)

print(virus_raw_data[rcount, ccount])




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
    insert_virus_stats(i,j,FALSE)
    #print(get_virus_stats(i,j,FALSE))
    j <- j+1
  }
  j <-2
  i <-i-1
}

#insert_virus_stats(49,2,TRUE)
#insert_virus_stats(8,3,TRUE)