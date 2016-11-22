rm(list=ls())
library(rethomics)
library(tools)
library(zoo)


  ############# Fetch data and add conditions

files <- list.files("/Users/diana/Dropbox/Phd Stuff/ESA/data_exp/all_db_mf", pattern="*.db",full.names = T)


#files <- list.files("/home/diana/Dropbox/Phd Stuff/ESA/data_exp/all_db_mf", pattern="*.db",full.names = T)
query <- data.table(path=files)

list_of_fields <- strsplit(basename(files),'[-.]')
fields_mat <- do.call("rbind", list_of_fields)

conditions <- as.data.table(fields_mat[,c(10,12)])
setnames(conditions, c("DAM","fly_id"))
query <- cbind(query, conditions)

dt <- loadEthoscopeData(query)
key(dt)

dt[, side := ifelse(x < 0.68, "left", "right")]
dt[, sleep_deprived := ifelse(DAM %in% c("005", "016", "004"), "sleep_deprived", "control")]
dt[, sex := ifelse(DAM %in% c("008", "016", "004", "009"), "female", "male")]

# All flies start at time 0.00
dt[, t:= t - min(t), by=key(dt)]

# get the same number of frames for each fly
same_time_dt <- dt[t < 53]

index_end_maze <- function(x, t) {
  index <- min(which(x > 0.68))
}

firstpart <- same_time_dt[, head(.SD, index_end_maze(x,t)), by=key(dt)]

interpol_time <- function(sub_dt){
  my_copy = copy(sub_dt)
  t_new = seq(from = min(my_copy[, t]), to=my_copy[.N, t], by = 0.033)
  time_map = data.table(t=t_new, key="t")
  setkeyv(my_copy, "t")
  out = my_copy[time_map, roll=T]
  out
}

new_fp <- firstpart[, interpol_time(.SD), by =key(firstpart)]


# be able to have miliseconds
options(digits.secs = 3)


# time as POSIXct for adehabitat
time <- as.POSIXct(new_fp[, t], origin = "1960-01-01")
traje <-as.ltraj(new_fp[, c("x","y"), with=FALSE], time, new_fp[, experiment_id], infolocs = new_fp[,c("sleep_deprived","sex"), with=FALSE])

# transform to data.frame and data.table
traje_df <- ld(traje)
traje_dt <- data.table(traje_df)


#PCA
traje_dt <- na.omit(traje_dt)
param_pca <- traje_dt[x <0.68,4:10, with=FALSE]
traje.pca <- prcomp(param_pca, center = TRUE, scale =TRUE)
print(traje.pca)
summary(traje.pca)
condition <-  traje_dt[x <0.68,sleep_deprived]
qplot(PC1, PC2, data = fortify(traje.pca), color = condition)
ggbiplot(traje.pca,choices=c(1,4), var.scale = 1)


test_dt <- fortify(traje.pca)
test_dt <- as.data.table(test_dt)
test_dt[, sleep_deprived := na.omit(traje_dt)[,sleep_deprived]]
test_dt


library(rgl)
plot3d(test_dt$PC1, test_dt$PC2, test_dt$PC3, ifelse(test_dt[,sleep_deprived]=='control', 'red', 'blue'))

