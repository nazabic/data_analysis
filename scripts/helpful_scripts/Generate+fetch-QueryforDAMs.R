# clean workspace
rm(list=ls())

# load rethomics
library(rethomics)

# define path to data files
DATA_PATH <- "/data/dam_daily"

# generate query for DMA data
query = data.table(start_date=c("2015-01-25"),
                   stop_date= c("2015-01-26"), 
                   machine_id=c("M004"))

# print query to screen
print(query)

# fetch data of query using function fetchDAMData
dt <- fetchDAMData(DATA_PATH, query, 
                   reference_hour = 9.0, 
                   FUN = sleepDAMAnnotation)
