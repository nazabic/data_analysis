rm(list=ls())
library(rethomics)
library(tools)


############# Fetch data and add complex(real=one.fly$x,imaginary=one.fly$y)

files <- list.files("/home/diana/Dropbox/Phd Stuff/ESA/data_exp/all_db_mf", pattern="*.db",full.names = T)
query <- data.table(path=files)

list_of_fields <- strsplit(basename(files),'[-.]')
fields_mat <- do.call("rbind", list_of_fields)

conditions <- as.data.table(fields_mat[,c(10,12)])
setnames(conditions, c("DAM","fly_id"))
query <- cbind(query, conditions)

dt <- loadPsvData(query)
key(dt)

dt[, side := ifelse(x < 0.68, "left", "right")]
dt[, sleep_deprived := ifelse(DAM %in% c("005", "016", "004"), "sleep_deprived", "control")]
dt[, sex := ifelse(DAM %in% c("008", "016", "004", "009"), "female", "male")]
dt[, points_chunck := floor(1:.N/20)*20, by=key(dt)]
dt[, angle := Arg(diff(complex(real=x, imaginary=y))) *180 /pi + 180, by= key(dt)]
dt[, cell:= floor(x*10 + 1)*10 + floor(y*10) ]

############ Get the data of each fly until it gets to the second part of the maze
index_end_maze <- function(x, t) {
        index <- min(which(x > 0.68))
}

firstpart <- dt[, head(.SD, index_end_maze(x,t)), by=key(dt)]


one.fly <- dt[experiment_id=='2015-08-11-Aug-13-1439288010-GGSM-005-DAM-005-FLY-8.db']
first.part.one.fly <- firstpart[experiment_id=='2015-08-11-Aug-13-1439288010-GGSM-005-DAM-005-FLY-8.db']
#plot trajectory in the maze


ggplot(one.fly,aes(phi, fill=sleep_deprived))+ geom_histogram(alpha=.5, position="identity", breaks=seq(0, 180 , by=5)) + xlab("Angles distributions")




plotFlyTrajectory <- function(fly){
        plot(fly$x, fly$y)
}


plotFlyTrajectory(first.part.one.fly)

plot(first.part.one.fly$x, first.part.one.fly$y)
par(new=T)
plot(first.part.one.fly[phi>=0 & phi <=15]$x, first.part.one.fly[phi>=0 & phi <=15]$y, col="blue")
par(new=T)
plot(first.part.one.fly[phi>=40 ]$x, first.part.one.fly[phi>=40]$y, col="green")

plot(first.part.one.fly$t, first.part.one.fly$angle)
plot(first.part.fly.2$t, first.part.fly.2$angle)



first.part.fly.2 <- firstpart[experiment_id=="2015-08-11-Aug-16-1439291802-GGSM-013-DAM-005-FLY-19.db"]

plot(first.part.fly.2$x,first.part.fly.2$y)


total_dist <- function(x,y){
        cplx <- x + 1.0i * y
        diff_cplx <- diff(cplx)
        all_distances <- abs(diff_cplx)
        return(sum(all_distances))
}

findSlope <- function(d){
        mod <- lm(y ~ x, d)
        a <- coef(mod)["x"]
        rsqr <- summary(mod)$r.square
        distance <- total_dist(d[,x], d[,y])
        list(a=a,rsqr=rsqr, distance=distance)
}




slope_dt <-one.fly[,
                        findSlope(.SD),
                        by=time_chunk]

traj <- complex(real=one.fly$x,imaginary=one.fly$y)
angles <- Arg(diff(traj))
angles <- c(NA,Arg(diff(traj)))
angles <- c(NA,Arg(diff(traj))) * 180/pi
angles <- c(NA,Arg(diff(traj)))
one.fly[, angle:=angles]

plot(one.fly$x, one.fly$angle)
plot(one.fly$angle)
plot(one.fly$angle,type = 'l')
plot(runmed(one.fly$angle,11),type = 'l')
plot(runmed(na.omit(one.fly$angle),11),type = 'l')
plot(runmed(na.omit(one.fly$angle),5),type = 'l')
plot(runmed(na.omit(sin(one.fly$angle)),5),type = 'l')
plot(runmed(sin(one.fly$angle)),type = 'l')
plot(sin(one.fly$angle),type = 'l')
plot( y ~ x, one.fly, col=rainbow(sin(angle)+1))
plot( y ~ x, na.omit(one.fly), col=rainbow(sin(angle)+1))
plot( y ~ x, na.omit(one.fly), col=rainbow(sin(angle)))
plot( y ~ x, na.omit(one.fly), col= sin(angle))
plot( y ~ x, na.omit(one.fly), col= heat.colors(sin(angle)))
ggplot(na.omit(one.fly[x<.68]), aes(x,y, col= sin(angle))) + geom_point()
ggplot(na.omit(one.fly[x<.68]), aes(x,y, col= cos(angle))) + geom_point()
ggplot(na.omit(one.fly[x<.68]), aes(x,y, col= cos(angle))<0) + geom_point()
ggplot(na.omit(one.fly[x<.68]), aes(x,y, col= cos(angle)<0)) + geom_point()
ggplot(na.omit(one.fly[x<.68]), aes(x,y, col= sin(angle))) + geom_point()
ggplot(na.omit(one.fly[x<.35]), aes(x,y, col= sin(angle))) + geom_point()
ggplot(na.omit(one.fly[x<.32]), aes(x,y, col= sin(angle))) + geom_point()
ggplot(na.omit(one.fly[x<.32 & x >.26]), aes(x,y, col= sin(angle))) + geom_point()
ggplot(na.omit(one.fly[x<.32 & x >.26]), aes(x,y, col= sin(angle))) + geom_point() + stat_smooth()
ggplot(na.omit(one.fly[x<.32 & x >.26]), aes(x,y, col= sin(angle))) + geom_point() + stat_smooth(method = lm)
mod <- lm(y ~ x, one.fly[x<.32 & x >.26])

findSlope(one.fly[x<.32 & x >.26])
one.fly[,time_chunk := floor(t/10) * 10]
one.fly
slope_dt <-one.fly[,
                   findSlope(.SD),
                   by=time_chunk]
slope_dt
slope_dt <-one.fly[,
                   findSlope(.SD),
                   by=time_chunk]
plot(a ~ time_chunk,slope_dt[rsqr > .8 & distance > 0.06] )


traj <- complex(real=one.fly$x,imaginary=one.fly$y)
one.fly[, angle := c(NA,Arg(diff(traj))* 180/pi) + 180]
one.fly[,time_chunk := floor(1:.N/30) * 30]
slope_dt <-one.fly[,
                   findSlope(.SD),
                   by=time_chunk]
slope_dt

plot(a ~ time_chunk, slope_dt[rsqr > .8 & distance > 0.06])



findSlope(one.fly[x<0.14])
findSlope(one.fly[x>0.16 & x < 0.2])
