library(rethomics)
library(tools)
library(ggplot2)
files <- list.files("/Users/diana/Desktop/males_Tuesday_db/all_db", pattern="*.db",full.names = T)
dt <- loadPsvData(files)

dt$dam <- apply(dt, 1, function(x) {strsplit(x[3], '-')[[1]][10]})
dt$flyidDam <- apply(dt, 1, function(x) {strsplit(x[3], '-')[[1]][12]})
dt$sleep_deprived <- apply(dt, 1, function(x) { if(x[11] == '015') {TRUE}
  else if (x[11] =="005") {FALSE}
  else {NA}})

#total distance function
total_dist <- function(x,y){
  cplx <- x + 1.0i * y
  diff_cplx <- diff(cplx)
  all_distances <- abs(diff_cplx)
  return(sum(all_distances))
}

distance <- function(x,y){
  cplx <- x + 1.0i * y
  diff_cplx <- diff(cplx)
  all_distances <- abs(diff_cplx)
  return(all_distances)
}

mean_velocity <- function(x,y,t){
  cplx <- x + 1.0i * y
  diff_cplx <- c(0,diff(cplx))
  all_velocities <- abs(diff_cplx)/t
  return(median(all_velocities))
}	

velocity <- function(x,y,t){
  diff_cplx <- c(0, diff(x))
  return(diff_cplx)
}	

# get data just for the first part of the maze

index_end_maze <- function(x, t) {
  index <- min(which(x > 0.68))
}

firstpart <- dt[, head(.SD, index_end_maze(x,t)), by=key(dt)]

firstpart$distance <- c(0, distance(firstpart$x, firstpart$y))
firstpart$velocity <- velocity(firstpart$x, firstpart$y, firstpart$t)


interesting_vars <- firstpart[,list(
  covered_dist = total_dist(x, y), velocity = mean_velocity(x, y, t)
  , time_finish = tail(t,1))
  ,by=c(key(dt), "sleep_deprived")]

# plot differences between plots in the first graph
time_density <- ggplot(interesting_vars, aes(time_finish, fill = sleep_deprived)) + geom_density(alpha = 0.2)
time_density
dist_density <- ggplot(interesting_vars, aes(covered_dist, fill = sleep_deprived)) + geom_density(alpha = 0.2)
dist_density
velocity_density <- ggplot(interesting_vars, aes(velocity, fill = sleep_deprived)) + geom_density(alpha = 0.2)
velocity_density
time_box <- ggplot(interesting_vars, aes(y=time_finish, x= sleep_deprived)) + geom_boxplot()
time_box
dist_box <-  ggplot(interesting_vars, aes(y=covered_dist, x= sleep_deprived)) + geom_boxplot()
dist_box
velocity_box <-  ggplot(interesting_vars, aes(y=velocity*110, x= sleep_deprived)) + geom_boxplot()
velocity_box


plot(firstpart$x, firstpart$t)
plot(firstpart$velocity, firstpart$t)

one_fly <- dt[experiment_id=="2015-08-11-Aug-16-1439291798-GGSM-005-DAM-005-FLY-20.db"]
interesting_vars_one_fly <- one_fly[,list(
  covered_dist = total_dist(x, y), velocity = mean_velocity(x, y, t)
  , time_finish = tail(t,1))
  ,by=c(key(dt), "sleep_deprived")]
first_part_one_fly <- head(one_fly, index_end_maze(one_fly$x,one_fly$t))
first_part_one_fly$distance <- c(0, distance(first_part_one_fly$x, first_part_one_fly$y))
first_part_one_fly$velocity <- velocity(first_part_one_fly$x, first_part_one_fly$y, first_part_one_fly$t)
plot(first_part_one_fly$x, -first_part_one_fly$y)
plot(first_part_one_fly$t, first_part_one_fly$x)

window <- rep(1/25, 25)
smoo <- sign(filter(first_part_one_fly$velocity, window, sides=2))
pattern <- c(-1, 1)
smoo <- smoo[!is.na(smoo)]

u_turn <- convolve(smoo, rev(pattern), type = "filter")


plot(first_part_one_fly$t[1:length(u_turn)], u_turn, type = "l")
plot(first_part_one_fly$distance, first_part_one_fly$t)



