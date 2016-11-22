rm(list=ls())
library(rethomics)
library(tools)
library(adehabitatLT)


############# Fetch data and add conditions

#files <- list.files("/Users/diana/Dropbox/Phd Stuff/ESA/data_exp/all_db_mf", pattern="*.db",full.names = T)


files <- list.files("/home/diana/Dropbox/Phd Stuff/ESA/data_exp/all_db_mf", pattern="*.db",full.names = T)
query <- data.table(path=files)

list_of_fields <- strsplit(basename(files),'[-.]')
fields_mat <- do.call("rbind", list_of_fields)

conditions <- as.data.table(fields_mat[,c(10,12)])
setnames(conditions, c("DAM","fly_id"))
query <- cbind(query, conditions)

dt <- loadEthoscopeData(query)
key(dt)

dt[, sleep_deprived := ifelse(DAM %in% c("005", "016", "004"), "sleep_deprived", "control")]
dt[, sex := ifelse(DAM %in% c("008", "016", "004", "009"), "female", "male")]

setkeyv(dt, c('experiment_id', 'region_id', 'DAM', 'fly_id', 'sleep_deprived', 'sex'))
# All flies start at time 0.00
dt[, t:= t - min(t), by=key(dt)]





interpol_time <- function(sub_dt){
  my_copy = copy(sub_dt)
  t_new = seq(from = min(my_copy[, t]), to=my_copy[.N, t], by = 1/30)

  #Create one zoo object for each variable that I want to interpolate
  x_new <- approx(sub_dt[, t], sub_dt[, x], t_new)$y
  y_new <- approx(sub_dt[, t], sub_dt[, y], t_new)$y
  w_new <- approx(sub_dt[, t], sub_dt[, w], t_new)$y
  h_new <- approx(sub_dt[, t], sub_dt[, h], t_new)$y
  
  data.table(x=x_new, y=y_new, w=w_new, h=h_new, t=t_new)
  
}

inter_dt <- dt[, interpol_time(.SD), by=key(dt)]

# one fly with points missing for 10 s
fly.miss <- dt[experiment_id == '2015-17-11-Aug-16-1439291802-GGSM-013-DAM-005-FLY-22.db']
fly.miss.new <- inter_dt[experiment_id == '2015-17-11-Aug-16-1439291802-GGSM-013-DAM-005-FLY-22.db']

plot(fly.miss$x ~ fly.miss$t,pch=20)
points(fly.miss.new$x ~ fly.miss.new$t,pch="x")

plot(fly.miss$y ~ fly.miss$t,pch=20)
points(fly.miss.new$y ~ fly.miss.new$t,pch="x")

# be able to have miliseconds
options(digits.secs = 3)


# time as POSIXct for adehabitat
time <- as.POSIXct(fly.miss.new[, t], origin = "1960-01-01")
traje <-as.ltraj(fly.miss.new[, c("x","y"), with=FALSE], time, fly.miss.new[, experiment_id], infolocs = fly.miss.new[,c("sleep_deprived","sex"), with=FALSE])

head(traje[[1]])

plot(traje)
