rm(list=ls())
library(rethomics)
library(tools)

files <- list.files("/Users/diana/Dropbox/Phd Stuff/ESA/data_exp/males_Tuesday_11_08_2015/all_dbs", pattern="*.db",full.names = T)
query <- data.table(path=files)

list_of_fields <- strsplit(basename(files),'[-.]')
fields_mat <- do.call("rbind", list_of_fields)
conditions <- as.data.table(fields_mat[,c(10,12)])
setnames(conditions, c("DAM","fly_id"))
query <- cbind(query, conditions)

dt <- loadPsvData(query)
key(dt)

dt[, side := ifelse(x < 0.68, "left", "right")]
dt[, sleep_deprived := ifelse(DAM=="005", TRUE, FALSE)]

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

# check if correct

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


interesting_vars <- right_dt[,list(
        covered_dist = total_dist(x, y), velocity = mean_velocity(x, y, t))
        ,by=c(key(dt), "sleep_deprived")]


dist_box <-  ggplot(interesting_vars, aes(y=covered_dist, x= sleep_deprived)) + geom_boxplot()
dist_box
velocity_box <-  ggplot(interesting_vars, aes(y=velocity*110, x= sleep_deprived)) + geom_boxplot()
velocity_box



setkeyv(right_dt,key(dt))
key(right_dt)

sum <- right_dt[,
         .(min_t = min(t),
           length_ = max(t) - min(t) ),
         by=key(right_dt)]

merge(longest_right_dt,sum)

