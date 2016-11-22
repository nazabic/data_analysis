library(rethomics)
library(tools)
library(rgl)
demo(hist3d)



"rgl"#files <- list.files("/home/diana/Desktop/test.db", pattern="*.db",full.names = T)
files <- "/data/Diana/everything_else/old_db_files/all_dbs/2015-08-13-Aug-53-1439459582-GGSM-003-DAM-004-FLY-6.db"
dt <- loadEthoscopeData(files)


plot(y ~ x, dt,pch=20,col=heat.colors(nrow(dt)))

plot(y ~ x, dt,pch=20,col=heat.colors(nrow(dt)))

getColor <- function(x) {
          if (diff(x) > 0) 
            {myColor <-  "red"} 
          else 
            {myColor <- "blue"}
  myColor
}

plot(t~x,dt,pch=20, col= ifelse(diff(x) > 0, "red", "black"))
plot3d(dt$y, dt$x, dt$t,pch=20,col=rainbow(nrow(dt)))


#dt$dam <- apply(dt, 1, function(x) {strsplit(x[3], '-')[[1]][9]})
#dt$dam_id <- apply(dt, 1, function(x) {strsplit(x[3], '-')[[1]][10]})




#3D histogram
rgl.open()
rgl.bg(col="#cccccc")
hist3d(dt$x, dt$y,alpha=0.4,nclass=30,scale=20)
title3d("One fly", xlab="x", zlab="y", ylab="time spent in one case")

# rgl.open()
# rgl.bg(col="#cccccc")
# hist3d(dt[sleep_deprived == 'control' & sex == 'female']$x, dt[sleep_deprived == 'control' & sex=='female']$y,alpha=0.4,nclass=50,scale=20)
# title3d("Female control flies ", xlab="x", zlab="y", ylab="time spent in one case")

rgl.open()
rgl.bg(col="#cccccc")
hist3d(dt[sleep_deprived == 'sleep_deprived' & sex == 'female']$x, dt[sleep_deprived == 'sleep_deprived' & sex=='female']$y,alpha=0.4,nclass=50,scale=20)
title3d("Female sleep deprived flies ", xlab="x", zlab="y", ylab="time spent in one case")

rgl.open()
rgl.bg(col="#cccccc")
hist3d(dt[sleep_deprived == 'control' & sex == 'male']$x, dt[sleep_deprived == 'control' & sex=='male']$y,alpha=0.4,nclass=30,scale=20)
title3d("Male control flies ", xlab="x", zlab="y", ylab="time spent in one case")

rgl.open()
rgl.bg(col="#cccccc")
hist3d(dt[sleep_deprived == 'sleep_deprived' & sex == 'male']$x, dt[sleep_deprived == 'sleep_deprived' & sex=='male']$y,alpha=0.4,nclass=30,scale=20)
title3d("Male sleep deprived flies ", xlab="x", zlab="y", ylab="time spent in one case")






