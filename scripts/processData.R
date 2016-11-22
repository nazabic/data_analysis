library(rethomics)
library(tools)
library(ggplot2)
files <- list.files("/Users/diana/Google\ Drive/Phd\ Stuff/maze_db_results", pattern="*.db",full.names = T)
dt <- loadPsvData(files)

dt$sex <- apply(dt, 1, function(x) {strsplit(x[3], '_')[[1]][1]})
dt$sleep_deprived <- apply(dt, 1, function(x) {strsplit(x[3], '_')[[1]][2]})
dt$id <-apply(dt, 1, function(x) {strsplit(x[3],"[_\\.]")[[1]][3]})

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
  all_velocities <- abs(diff_cplx)/t
  return(median(all_velocities))
}	

exploratoryScore <- function(y, t) {
  score <- 0
  for(i in seq_along(y)) {
    if (!is.na(y[i])  && !is.na(y[i+1]) && y[i]< y[i+1]) {
      score <- score + 1
    }
  }
  score/(max(t) - min(t))
}


interesting_vars <- dt[,list(
  covered_dist = total_dist(x,y), velocity = mean_velocity(x, y, t), exploratoryScore = exploratoryScore(y,t), delta_t = max(t) - min(t))
  ,by=c(key(dt),"sex","sleep_deprived")]

# 
# #Plot total distance males against females
# p1 <- ggplot(interesting_vars, aes(covered_dist, fill = sex)) + geom_density(alpha = 0.2)
# 
# #Plot total distance sleep_deprived vs normal
# p2 <- ggplot(interesting_vars, aes(covered_dist, fill = sleep_deprived)) + geom_density(alpha = 0.2)
# 
# #time spent in the maze sleep_deprived vs normal
# p3 <- ggplot(interesting_vars, aes(delta_t, fill = sleep_deprived)) + geom_density(alpha = 0.2)
# 
# #time spent in the maze males vs females
# p4 <- ggplot(interesting_vars, aes(delta_t, fill = sex)) + geom_density(alpha = 0.2)
# 
# #covered distance against sleep deprived
# p5 <- ggplot(interesting_vars, aes(y=covered_dist,x=sleep_deprived,fill=sex)) + geom_boxplot()
# 
# #velocity denisty
# p6 <- ggplot(interesting_vars, aes(velocity, fill = sleep_deprived)) + geom_density(alpha = 0.2)
# 
# #delta_t vc sleep deprived
# p7 <- ggplot(interesting_vars, aes(y=delta_t,x=sleep_deprived,fill=sex)) + geom_boxplot()
# 
# #females exploratory score
# p8 <- ggplot(females, aes(exploratoryScore, fill = sleep_deprived)) + geom_density(alpha = 0.2)

# get the minimum and maximum x and y values so that we have a rough idea about the length of the maze
minX <- min(dt[sex =='female' & sleep_deprived == TRUE]$x)
maxX <- max(dt[sex =='female' & sleep_deprived == TRUE]$x)
minY <- min(dt[sex =='female' & sleep_deprived == TRUE]$y)
maxY <- max(dt[sex =='female' & sleep_deprived == TRUE]$y)

#width of the maze
width <- maxX- minX

#height
height <- maxY-minY


maxWidthCm <- 7.8
maxHeightCm <- 8.1



#slect rows
#dt[dt$sex == 'female' & id == 1]


#where in equivalent
#result <- dt[dt$id %in%  c(1,2,3)]

#dt[,list(walked_dist = foo(.SD)),by=key(dt)]
#foo <- function(d){pos = d[,x] + i*d[,y];dists <- abs(diff(pos)); sum(dists)}
#delta_t <- dt[,list(delta_t=max(t)-min(t)),by=key(dt)]


#Get a certain individual data -example:female 1 not sleep deprived
#f1 <- subset(dt, id==1 & sleep_deprived==FALSE & sex=='female')


