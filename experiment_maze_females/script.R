library(rethomics)
library(animation)
setwd("/data/Diana/analyze/experiment_maze_females")
q <- fread("./query3.csv")
q <- buildEthoscopeQuery("/data/Diana/data_node/ethoscope_videos", q)
dt <- loadEthoscopeData(q)

# time each fly spent in the maze
dt[, .(t_maze = tail(.SD$t, 1)/60), by=key(dt)]

# remove the initial points of tracking that are eronous tracking. 

dt_clean <- dt[!(t  <  30 & x > 0.68)]
dt_clean <- dt_clean[!(x < 0.2 & y > 0.4)]
dt_clean <- dt_clean[!(x < 0.2 & y < 0.3)]

# 15 minutes of data for each fly 
dt_15 <- dt[t < 900, .SD ,by=key(dt)]

dt_15 <- dt_clean[t < 900, .SD ,by=key(dt)]
dt_15[, .(t_maze = tail(.SD$t, 1)/60), by=key(dt_15)]


#ggplot(dt_15, aes(x, y, col=t)) + geom_point() + facet_grid(round ~ uid) 

dt_15 <- dt_15[, .(x=x, y=y, phi=phi, t=t, w=w, h=h, vx = c(0, diff(.SD$x)/diff(.SD$t)), uid= paste(dam, experiment, fly_id, sd, sep='_')), by=key(dt_15)]
dt <- dt[, .(x=x, y=y, phi=phi, t=t, w=w, h=h, vx = c(0, diff(.SD$x)/diff(.SD$t)), uid= paste(dam, experiment, fly_id, sd, sep='_')), by=key(dt)]

#dt_15[, uid:=paste(dam, experiment, fly_id, sep='_')]



dt_one_fly <-dt_15[uid=="3_4_10_TRUE"]


ggplot(dt_one_fly, aes(x, -y, col=t)) + geom_point() + facet_grid(round ~ uid) 
#function to draw the scatterplot, but the curve fit only up to whatever index we set it at
draw.curve<-function(cutoff){
  a<-ggplot(dt_one_fly[dt_one_fly$t<cutoff], aes(x, -y, col=t)) + geom_point() +
    scale_x_continuous(limits=c(0, 1)) + 
    scale_y_continuous(limits=c(-1, 0))
  print(a)
}

#try it out: draw curve up to cutoff x-value of -2
draw.curve(cutoff=500)


#function to iterate over the full span of x-values
trace.animate <- function() {
  lapply(seq(8,899, 1), function(i) {
    draw.curve(i)
  })
}

#save all iterations into one GIF
saveGIF(trace.animate(), interval = 0.05, movie.name="trace005.gif")


ggplot(dt_one_fly, aes(x=phi)) + geom_histogram(binwidth=10) + coord_polar(start = 0) + facet_grid(round ~ uid)

p <- dt_15[, .(in_maze=was_in_maze(.SD$x), in_open_arena=was_in_open_arena(.SD$x)), by=key(dt_15)]


# To compute percent of flies that went in maze:
sum(p[sd==T]$in_maze)/length(p[sd==T]$in_maze)*100
sum(p[sd==F]$in_maze)/length(p[sd==F]$in_maze)*100

# To compute percentage of flies that were in the open arena
sum(p[sd==T]$in_open_arena)/length(p[sd==T]$in_open_arena)*100
sum(p[sd==F]$in_open_arena)/length(p[sd==F]$in_open_arena)*100


#ggplot(dt_15, aes(t, vx )) + geom_point() + facet_grid(round ~ uid)

ggplot(dt_one_fly, aes(t, vx )) + geom_point() + facet_grid(round ~ uid)
plot(dt_one_fly$vx, type='l')

stepsize = 25
vx_downsampled <-  c(0,diff(dt_one_fly$x[seq(1,length(dt_one_fly$x),by=stepsize)])/diff(dt_one_fly$t[seq(1,length(dt_one_fly$t),by=stepsize)]))
plot(vx_downsampled, type="l")

# look for a threshold for the velocity, when do they stopped 
qplot(vx_downsampled, geom='histogram')
qplot(vx_downsampled[vx_downsampled < 0.006 & vx_downsampled > -0.006], geom="histogram", bins= 10)

dt_one_fly[, vx_resampled := approx(dt_one_fly$t[seq(1,length(dt_one_fly$x),by=stepsize)], vx_downsampled, xout=dt_one_fly$t)[2]]
vy_downsampled <-  c(0,diff(dt_one_fly$y[seq(1,length(dt_one_fly$y),by=stepsize)])/diff(dt_one_fly$t[seq(1,length(dt_one_fly$y),by=stepsize)]))
plot(vy_downsampled, type="l")

dt_one_fly[, vy_resampled := approx(dt_one_fly$t[seq(1,length(dt_one_fly$y),by=stepsize)], vy_downsampled, xout=dt_one_fly$t)[2]]
dt_one_fly[, v_resampled :=  sqrt(vx_resampled^2+vy_resampled^2)]
ggplot(dt_one_fly, aes(x, -y, col=vx_resampled*sign(cos(phi)))) + geom_point() + facet_grid(round ~ uid) + scale_colour_gradient(low="yellow", high="blue",limits=c(-0.02,0.07))
ggplot(dt_one_fly, aes(x, -y, col=v_resampled)) + geom_point() + facet_grid(round ~ uid) + scale_colour_gradient(low="yellow", high="blue")
ggplot(dt_one_fly, aes(t,v_resampled)) + geom_line() + facet_grid(round ~ uid) + scale_colour_gradient(low="yellow", high="blue",limits=c(-0.02,0.07))


ggplot(dt_one_fly, aes(x, -y, col=vx_resampled*sign(cos(phi)))) + geom_point() + facet_grid(round ~ uid) + scale_colour_gradient(low="red", high="blue")


was_in_maze <- function(x)
{
  if(sum(x>0.19) > 100)
    return(T)
  else
    return(F)
}

was_in_open_arena <-function(x)
{
  if(sum(x>0.68 & x < 0.8) > 100)
    return(T)
  else
    return(F)
}

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

m_velocity <- function(x,y,t){
  cplx <- x + 1.0i * y
  diff_cplx <- c(0,diff(cplx))
  all_velocities <- abs(diff_cplx)/c(NA,diff(t))
  all_velocities = na.omit(all_velocities)
  return(mean(all_velocities))
}




# All flies start at time 0.00
#dt_15[, t:= t - min(t), by=key(dt)]



############ Get the data of each fly until it gets to the second part of the maze
index_end_maze <- function(x, t) {
  index <- min(which(x > 0.68))
}

firstpart <- dt_15[, head(.SD, index_end_maze(x,t)), by=key(dt)]
ggplot(firstpart, aes(x=phi, col=sd)) + geom_histogram(binwidth=10) + coord_polar(start = 0) #+ facet_grid(round ~ uid)


lastPoints <- firstpart[x>0.67 & x<0.68]
lastPoints[,lastx:= mean(.SD$x), by=key(lastPoints)]
lastPoints[,lasty:= mean(.SD$y), by=key(lastPoints)]

plot(-y~x, firstpart) 
par(new=T)
points(lastPoints$lastx, -lastPoints$lasty, col="blue")
plot(lastPoints$lasty)
abline(h=0.055)
abline(h=0.08)


interesting_vars <- firstpart[,list(
  covered_dist = total_dist(x, y), velocity = mean_velocity(x, y, t), mean_v= m_velocity(x, y ,t), time_finish = tail(t,1)-head(t)),by=key(firstpart)]


myTheme <-  theme(axis.text.x = element_text(size=21, face = "bold"), axis.title.y = element_text(size=21, face = "bold"), legend.text = element_text(size=17), legend.title = element_text(size=18), plot.title = element_text(size = 23, face = "bold"))
ggplot(interesting_vars, aes(y=velocity*112,x=round,fill=sd)) + geom_boxplot() + xlab("") + ylab("Mean velocity in the maze (mm/s)") + ylim(0.2, 1)

ggplot(interesting_vars, aes(y=time_finish,x=round,fill=sd)) + geom_boxplot() + xlab("") + ylab("Time (s)") + myTheme + ggtitle("Time spent in the maze") + scale_fill_discrete(name="Experimental\nCondition",
                                                                                                                                                                                          breaks=c("control", "sleep_deprived"),
                                                                                                                                                                                          labels=c("control", "sleep deprived"))
ggplot(interesting_vars, aes(y=covered_dist*112,x = round, fill = sd)) + geom_boxplot() + xlab("") + ylab("Distance (mm)") + myTheme + ggtitle("Total distance walked in the maze")

ggplot(interesting_vars, aes(y=velocity*112,x=sd)) + geom_boxplot() + xlab("") + ylab("Median velocity \ in the first part of the maze (mm/s)") + ylim(0.2, 1)
ggplot(interesting_vars, aes(y=mean_v*112,x=sex,fill=sd)) + geom_boxplot() + xlab("") + ylab("Mean velocity (mm/s)") + ylim(0.2, 1)



library(ggplus)
p <- ggplot(dt_15, aes(x, -y, col=t)) + geom_point()
p <- facet_multiple(plot=p, facets=c("uid"), nrow=4, ncol=4) 


lastPoints <- firstpart[x>0.67 & x<0.68 ]
lastPositions <- lastPoints[, list(lastx=mean(.SD$x), lasty=mean(.SD$y)), by=key(lastPoints)]

plot(lastPoints$x, lastPoints$y)

exitNumber <- function(y) {
  exit <- 0
  if (y < 0.15) { exit <- 1 }
  else if (y > 0.15 & y < 0.29) { exit <- 2}
  else if (y > 0.29 & y < 0.35)  {exit <- 3}
  else if (y > 0.35 & y < 0.47)  {exit <- 4}
  else if (y >0.47 & y < 0.57) {exit <- 5}
  else if (y > 0.57) {exit <- 6}
  exit
}

lastPositions[, exit:=sapply(lasty, exitNumber)]

hist(lastPositions$exit, col='red', breaks = 20, main="Exit distribution of ALL flies", xlab='exit #')


par(new=T)
d <- density(lastPositions$exit)
plot(d)

ggplot(data=lastPositions, aes(lastPositions$exit)) + geom_histogram(aes(y = ..density..)) + geom_density()


# test for normal distribution
shapiro.test(lastPositions$exit)
qqnorm(lastPositions$exit)




