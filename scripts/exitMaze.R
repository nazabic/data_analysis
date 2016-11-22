rm(list=ls())
library(rethomics)
library(tools)


############# Fetch data and add conditions
files <- list.files("/Users/diana/Dropbox/Phd Stuff/ESA/data_exp/all_db_mf", pattern="*.db",full.names = T)

query <- data.table(path=files)

list_of_fields <- strsplit(basename(files),'[-.]')
fields_mat <- do.call("rbind", list_of_fields)

conditions <- as.data.table(fields_mat[,c(3,8,10,12)])
setnames(conditions, c("day","machine","DAM","fly_id"))
query <- cbind(query, conditions)

dt <- loadPsvData(query)
key(dt)

dt[, side := ifelse(x < 0.68, "left", "right")]
dt[, sleep_deprived := ifelse(DAM %in% c("005", "016", "004"), "sleep_deprived", "control")]
dt[, sex := ifelse(DAM %in% c("008", "016", "004", "009"), "female", "male")]


############ Get the data of each fly until it gets to the second part of the maze
index_end_maze <- function(x, t) {
  index <- min(which(x > 0.68))
}

firstpart <- dt[, head(.SD, index_end_maze(x,t)), by=key(dt)]

lastPoints <- firstpart[x>0.67 & x<0.68 & (day %in% c('11','12','13'))]
lastPositions <- lastPoints[, list(lastx=mean(.SD$x), lasty=mean(.SD$y)), by=key(lastPoints)]

exitNumber <- function(y) {
              exit <- 0
              if (y > 0.05 & y <0.09) { exit <- 1 }
              else if (y > 0.15 & y < 0.2) { exit <- 2}
              else if (y > 0.25 & y < 0.3)  {exit <- 3}
              else if (y > 0.35 & y < 0.4)  {exit <- 4}
              else if (y >0.45 & y < 0.52) {exit <- 5}
              else if (y >0.55 & y < 0.6) {exit <- 6}
              exit
}

lastPositions[, exit:=sapply(lasty, exitNumber)]

hist(lastPositions$exit, col='red', breaks = 20, main="Exit distribution of ALL flies", xlab='exit #')


hist(tail(lastPositions$exit,15), breaks=20, col="red")



par(new=T)
d <- density(lastPositions$exit)
plot(d)

ggplot(data=lastPositions, aes(lastPositions$exit)) + geom_histogram(aes(y = ..density..)) + geom_density()


# test for normal distribution
shapiro.test(lastPositions$exit)
qqnorm(lastPositions$exit)




exitByFly <- lastPoints[, list(exit=lapply(lasty, exitNumber)), by=key(lastPoints)] 
plot(lastPositions$exit)

plot(-y~x, firstpart) 
par(new=T)
points(lastPoints$lastx, -lastPoints$lasty, col="blue")

plot(lastPoints$lasty)
abline(h=0.05)
abline(h=0.09)


abline(h=0.15)
abline(h=0.2)

abline(h=0.25)
abline(h=0.3)

abline(h=0.35)
abline(h=0.4)

abline(h=0.45)
abline(h=0.52)

abline(h=0.55)
abline(h=0.6)

myFly <- firstpart[experiment_id=="2015-08-13-Aug-44-1439462668-GGSM-005-DAM-004-FLY-20.db"]

myFly<-firstpart[experiment_id=="2015-08-11-Aug-13-1439288014-GGSM-013-DAM-005-FLY-7.db"]


find_Uturns <- function(velocity,window_size) {
  window <- rep(1/window_size, window_size)
  out_vel = filter(velocity, window, sides=2)
  smoo <- sign(filter(velocity, window, sides=2))
  pattern <- c(-1, 1)
  smoo <- smoo[!is.na(smoo)]
  u_turn <- convolve(smoo, rev(pattern), type='filter')
  list(u_turn,out_vel)
}

listout = find_Uturns(diff(myFly$x),25)
u_turn = listout[[1]]
smoothed_velocity = listout[[2]]
plot(myFly$t[1:length(diff(myFly$x))], diff(myFly$x),type = "l")
plot(myFly$t[1:length(smoothed_velocity)], smoothed_velocity,type = "l")
plot(myFly$t[1:length(u_turn)], u_turn, type = "l", ylim=c(-2,2))
sum(u_turn >1.9)


turns <- firstpart[, list(uturns=sum(find_Uturns(diff(x),9)[[1]]>1.9)), by=c(key(firstpart), "sleep_deprived", "sex")] 

hist(turns[sex=='female' & sleep_deprived =='control']$uturns)  
turns_box <-  ggplot(turns, aes(y=uturns, x= sex, fill=sleep_deprived)) + geom_boxplot() +xlab("") + ylab("Mean velocity in the second part of the maze (mm/s)")


