rm(list=ls())
library(rethomics)
library(tools)


############# Fetch data and add conditions
#files <- list.files("/Users/diana/Dropbox/Phd Stuff/ESA/data_exp/all_db_mf", pattern="*.db",full.names = T)
files <- list.files("/home/diana/Dropbox/Phd Stuff/ESA/data_exp/all_db_mf", pattern="*.db",full.names = T)
query <- data.table(path=files)

list_of_fields <- strsplit(basename(files),'[-.]')
fields_mat <- do.call("rbind", list_of_fields)

conditions <- as.data.table(fields_mat[,c(10,12)])
setnames(conditions, c("DAM","fly_id"))
query <- cbind(query, conditions)

dt <- loadPsvData(query)

ggplot(dt , aes(x,y)) + stat_density2d(aes(fill=..level..),geom="polygon")
pl = ggplot(dt , aes(x,y)) + stat_density2d(aes(fill=..level..),geom="polygon")
pl + scale_fill_gradient(low="blue", high="green")

ggplot(dt , aes(x,y)) + geom_hex()
scale_fill_gradient(low="blue", high="green")

ggplot(dt , aes(x,y)) + geom_hex()
ggplot(dt , aes(x,y)) + geom_hex() + facet_grid( sleep_deprived ~ sex)
