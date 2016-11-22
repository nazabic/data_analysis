library(rethomics)
library(tools)
library(ggplot2)
library(rgl)

"rgl"#files <- list.files("/home/diana/Desktop/test.db", pattern="*.db",full.names = T)
files <- "/home/diana/Desktop/test.db"
dt <- loadPsvData(files)

plot(y ~ x, dt,pch=20,col=heat.colors(nrow(dt)))
plot3d(dt$y, dt$x, dt$t,pch=20,col=rainbow(nrow(dt)))


#dt$dam <- apply(dt, 1, function(x) {strsplit(x[3], '-')[[1]][9]})
#dt$dam_id <- apply(dt, 1, function(x) {strsplit(x[3], '-')[[1]][10]})

