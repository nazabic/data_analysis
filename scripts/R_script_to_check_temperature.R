library(data.table)
library(ggplot2)

#' Unix time stammp to UTC date
asDateTime <- function(unixts){
  as.POSIXct(unixts, origin="1970-01-01",tz = "UTC")
}

URL <- "ftp://etho-node.lab.gilest.ro/ardufly-data/smart_incubator.csv"

# directly load data from ftp!
dt <- fread(URL)

change unix time stamp to date objects
dt[, device_time:=asDateTime(device_time)]
dt[, server_time:=asDateTime(server_time)]

# just a few exemples, showing one variable vs time one facet per incubator
# show temperature
ggplot(dt,aes(server_time, temperature)) + geom_line() + geom_point() + facet_grid(id ~ .)

#show_light
ggplot(dt,aes(server_time, light)) + geom_line() + 
                                   geom_point() + facet_grid(id ~ .) +
                                  theme(axis.text.x = element_text(angle=90,vjust = 0.3))
