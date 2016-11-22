# clean workspace
rm(list=ls())

# load rethomics
library(rethomics)

# define path to data files
DATA_PATH <- "/data/dam_daily"

# define path to query file
QUERY_PATH <- "path_to_file/my_file.csv"

# read query into R
query <- fread(QUERY_PATH, sep = ",")

# print query to screen
print(query)

# fetch data of query using function fetchDAMData
dt <- fetchDAMData(DATA_PATH, query, 
                   reference_hour = 9.0, 
                   FUN = sleepDAMAnnotation)
