rm(list=ls())
library(rethomics)
library(tools)


############# Fetch data and add conditions
#files <- list.files("/Users/diana/Dropbox/Phd Stuff/ESA/data_exp/all_db_mf", pattern="*.db",full.names = T)
files <- list.files("/home/diana/Desktop/all_dbs", pattern="*.db",full.names = T)
query <- data.table(path=files)

list_of_fields <- strsplit(basename(files),'[-.]')
fields_mat <- do.call("rbind", list_of_fields)

conditions <- as.data.table(fields_mat[,c(10,12)])
setnames(conditions, c("DAM","fly_id"))
query <- cbind(query, conditions)

dt <- loadEthoscopeData(query[1:10])
key(dt)

dt[, side := ifelse(x < 0.68, "left", "right")]
dt[, sleep_deprived := ifelse(DAM %in% c("005", "016", "004"), "sleep_deprived", "control")]
dt[, sex := ifelse(DAM %in% c("008", "016", "004", "009"), "female", "male")]
dt[, "angle"] <- atan(c(0,diff(dt[,y])/diff(dt[,x]))) * 180/pi


############ Write functions that you want to analyse

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
dt[, t:= t - min(t), by=key(dt)]

# the shortest path fly => 53 s
short_path_fly <- dt[experiment_id=="2015-08-12-Aug-16-1439378174-GGSM-003-DAM-008-FLY-17.db"]

# has 1316 rows, frames in the video
min_n_rows <- nrow(short_path_fly)

# get the same number of frames for each fly
same_time_dt <- dt[t < 53]



same_time_dt[, color:=ifelse(x < 0.68, 0, 1)]

summary_dt <- same_time_dt[, list(time_var = max(t) - min(t)), by=c(key(dt), "color")]

############ Get the data of each fly until it gets to the second part of the maze
index_end_maze <- function(x, t) {
  index <- min(which(x > 0.68))
}

firstpart <- dt[, head(.SD, index_end_maze(x,t)), by=key(dt)]

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
  covered_dist = total_dist(x, y), velocity = mean_velocity(x, y, t), mean_v= m_velocity(x, y ,t), time_finish = tail(t,1)-head(t))
  ,by=c(key(firstpart), "sex", "sleep_deprived")]

interesting_vars_dt <- dt[,list(
        covered_dist = total_dist(x, y), velocity = mean_velocity(x, y, t), mean_v= m_velocity(x, y ,t), time_finish = tail(t,1)-head(t))
        ,by=c(key(firstpart), "sex", "sleep_deprived")]

myTheme <-  theme(axis.text.x = element_text(size=21, face = "bold"), axis.title.y = element_text(size=21, face = "bold"), legend.text = element_text(size=17), legend.title = element_text(size=18), plot.title = element_text(size = 23, face = "bold"))
 ggplot(interesting_vars, aes(y=velocity*112,x=sex,fill=sleep_deprived)) + geom_boxplot() + xlab("") + ylab("Mean velocity in the maze (mm/s)") 
 
 ggplot(interesting_vars, aes(y=time_finish,x=sex,fill=sleep_deprived)) + geom_boxplot() + xlab("") + ylab("Time (s)") + myTheme + ggtitle("Time spent in the maze") + scale_fill_discrete(name="Experimental\nCondition",
                                                                                                                                                                                           breaks=c("control", "sleep_deprived"),
                                                                                                                                                                                           labels=c("control", "sleep deprived"))
 ggplot(interesting_vars, aes(y=covered_dist*112, x=sex, fill = sleep_deprived)) + geom_boxplot() + xlab("") + ylab("Distance (mm)") + ylim(85,110) + myTheme + ggtitle("Total distance walked in the maze")

 ggplot(interesting_vars, aes(y=velocity*112,x=sleep_deprived)) + geom_boxplot() + xlab("") + ylab("Median velocity \ in the first part of the maze (mm/s)") 
 ggplot(interesting_vars, aes(y=mean_v*112,x=sex,fill=sleep_deprived)) + geom_boxplot() + xlab("") + ylab("Mean velocity (mm/s)") 
 
 
 
oneFly <-  firstpart[experiment_id=="2015-08-13-Aug-18-1439461101-GGSM-005-DAM-004-FLY-11.db"]
plot(diff(x)~t[1:(length(x)-1)],oneFly,pch=20, col= ifelse(diff(x) > 0, "red", "black"))


plot(-y~x,oneFly) 
par(new=T)
points(oneFly[x>0.67 & x<0.68]$x,-oneFly[x>0.67 & x<0.68]$y, col="blue")

 ########## get mean velocity in the second part of the maze
 
 bout_dt <- boutAnalysis(side,dt)
 
 findLongestRightStay <- function(sub_data){
   sdt <- copy(sub_data)
   sdt <- sdt[side=="right"]
   longest_idx <- which.max(sdt[,length])
   sdt[longest_idx,]
 }
 
 longest_right_dt <- bout_dt[, 
                             findLongestRightStay(.SD)        
                             ,by=key(bout_dt)]
 
 getDataBack <- function(i,bout_dt,data){
   sdt <- bout_dt[i,]
   start <- sdt[,start_time]
   end <- start + sdt[,length]
   sdt <- sdt[,key(sdt),with=F]
   out <- copy(data[sdt])
   out[t > start & t < end]
 }
 
 l_dts <- lapply(1:nrow(longest_right_dt), 
                 getDataBack, 
                 bout_dt=longest_right_dt, data=dt )
 
 right_dt <- rbindlist(l_dts)
 
 interesting_vars_right <- right_dt[,list(velocity_right = mean_velocity(x, y, t), m_velocity_right = m_velocity(x, y, t))
   ,by=c(key(dt), "sleep_deprived", "sex")]
 
 
 velocity_box <-  ggplot(interesting_vars_right, aes(y=velocity_right*112, x= sex, fill=sleep_deprived)) + geom_boxplot() +xlab("") + ylab("Mean velocity in the field (mm/s)") 
 velocity_box

 mean_box <-  ggplot(interesting_vars_right, aes(y=m_velocity_right*112, x= sex, fill=sleep_deprived)) + geom_boxplot() +xlab("") + ylab("Mean velocity (mm/s)") 
 mean_box
 
 
 both_parts <- merge(interesting_vars, interesting_vars_right)
 
# Get 56s (the minimum) of time for all flies
 
 
 overviewPlot <- function(y,data,
                          condition=NULL,
                          summary_time_window=1){
   
   
   dt = copy(as.data.table(data))  
   y_var_name <- deparse(substitute(y))
   setnames(dt,y_var_name,"y_var")
   dt[,t_r := floor(t/summary_time_window) * summary_time_window]
   dt[,y_var:=as.numeric(y_var)]
   c_var_name <- deparse(substitute(condition))
   
   if(c_var_name == "NULL")
     dt[,c_var:=TRUE]
   else
     setnames(dt, c_var_name,"c_var")
   
   summary_dt <- dt[,list(y_var=mean(y_var)),
                    by=c("t_r","c_var",key(dt))]
   summary_dt[,t_d:=t_r]
   
   if(c_var_name != "NULL"){
     summary_dt[,row_name:=sprintf("%s | %s | %02d",c_var,experiment_id,region_id)]
     y_lab <- sprintf("Individual (%s | experiment_id | region_id)", c_var_name)
   }
   else{
     summary_dt[,row_name:=sprintf("%s | %02d",experiment_id,region_id)]
     y_lab <- "Individual (experiment_id | region_id)"
   }
   
   p <- ggplot(summary_dt,aes(x=t_d,y=row_name,fill=y_var)) + geom_tile(alpha=1) +
     labs(title= sprintf("Overview of individual pattern over time"),x="time (s)", y=y_lab)+
     guides(fill=guide_legend(title=y_var_name))
   p
 }
 
 
 
 ggplot(dt , aes(x,y)) + stat_density2d(aes(fill=..level..),geom="polygon")
 