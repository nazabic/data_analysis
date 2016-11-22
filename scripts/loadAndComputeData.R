rm(list=ls())
library(rethomics)
library(tools)
library(ggplot2)
files <- list.files("/Users/diana/Dropbox/Phd Stuff/maze_db_results", pattern="*.db",full.names = T)
dt <- loadPsvData(files)
query <- data.table(path=files)

# dt$sex <- apply(dt, 1, function(x) {strsplit(x[2], '_')[[1]][1]})
# dt$sleep_deprived <- apply(dt, 1, function(x) {strsplit(x[2], '_')[[1]][2]})
# dt[, sleep_deprived := ifelse(sleep_deprived == 'TRUE', "sleep_deprived", "control")]
# dt$id <-apply(dt, 1, function(x) {strsplit(x[2],"[_\\.]")[[1]][3]})


list_of_fields <- strsplit(basename(files),'[-._]')
fields_mat <- do.call("rbind", list_of_fields)

conditions <- as.data.table(fields_mat[,c(1,2,3)])
setnames(conditions, c("sex","sleep_deprived","fly_id"))
query <- cbind(query, conditions)
dt <- loadPsvData(query)
dt[, sleep_deprived := ifelse(sleep_deprived=='TRUE', "sleep_deprived", "control")]

#total distance function
total_dist <- function(x,y){
  cplx <- x + 1.0i * y
  diff_cplx <- diff(cplx)
  all_distances <- abs(diff_cplx)
  return(sum(all_distances))
}

mean_velocity <- function(x,y,t){
  cplx <- x + 1.0i * y
  diff_cplx <- c(0,diff(cplx))
  all_velocities <- abs(diff_cplx)/c(NA,diff(t))
  all_velocities = na.omit(all_velocities)
  return(median(all_velocities))
}	

exploratoryScore <- function(y, t) {
  score <- 0
  for(i in seq_along(y)) {
    if (!is.na(y[i])  && !is.na(y[i+1]) && y[i]< y[i+1]) {
      score <- score + 1
    }
  }
  score/length(y)
}


#Transform position in cm

# get the minimum and maximum x and y values so that we have a rough idea about the length of the maze
minX <- min(dt[sex =='female' & sleep_deprived == 'sleep_deprived']$x)
maxX <- max(dt[sex =='female' & sleep_deprived == 'sleep_deprived']$x)
minY <- min(dt[sex =='female' & sleep_deprived == 'sleep_deprived']$y)
maxY <- max(dt[sex =='female' & sleep_deprived == 'sleep_deprived']$y)

#width of the maze
width <- maxX- minX

#height
height <- maxY-minY

# 0.5140622 correspond to 7.8cm therefore 1 unit corresponds to 15.17326
maxWidthCm <- 7.8
maxHeightCm <- 8.1


interesting_vars <- dt[,list(
  covered_dist = total_dist(x,y), velocity = mean_velocity(x, y, t),  exploratoryScore = exploratoryScore(y,t), delta_t = max(t) - min(t))
  ,by=c("experiment_id","sex","sleep_deprived")]

interesting_vars$covered_dist_cm <- interesting_vars$covered_dist*15.17326

find_Uturns <- function(velocity,window_size) {
  window <- rep(1/window_size, window_size)
  out_vel = filter(velocity, window, sides=2)
  smoo <- sign(filter(velocity, window, sides=2))
  pattern <- c(-1, 1)
  smoo <- smoo[!is.na(smoo)]
  u_turn <- convolve(smoo, rev(pattern), type='filter')
  list(u_turn,out_vel)
}
turns <- dt[, list(uturns=sum(find_Uturns(diff(y),25)[[1]]>1.9)), by=c(key(dt), "sleep_deprived", "sex")] 

hist(turns[sex=='female' & sleep_deprived =='control']$uturns)  
turns_box <-  ggplot(turns[sex=='female'], aes(y=uturns, x= sleep_deprived)) + geom_boxplot() +xlab("") + ylab("Mean velocity in the second part of the maze (mm/s)")
wilcox.test(turns[sex=='female' & sleep_deprived =='control']$uturns, turns[sex=='female' & sleep_deprived =='sleep_deprived']$uturns, paired=T)

