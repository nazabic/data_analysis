rm(list=ls())
library(rethomics)
library(tools)
library(adehabitatLT)
library()


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
dt[, sleep_deprived := ifelse(DAM %in% c("005", "016", "004"), "sleep_deprived", "control")]
dt[, sex := ifelse(DAM %in% c("008", "016", "004", "009"), "female", "male")]


interpol_time <- function(sub_dt){
  my_copy = copy(sub_dt)
  t_new = seq(from = min(my_copy[, t]), to=my_copy[.N, t], by = 0.033)
  time_map = data.table(t=t_new, key="t")
  setkeyv(my_copy, "t")
  out = my_copy[time_map, roll=T]
  out
}

new_dt <- dt[, interpol_time(.SD), by =key(dt)]




# be able to have miliseconds
options(digits.secs = 3)


# time as POSIXct for adehabitat
time <- as.POSIXct(new_dt[, t], origin = "1960-01-01")
traje <-as.ltraj(new_dt[, c("x","y"), with=FALSE], time, new_dt[, experiment_id], infolocs = new_dt[,c("sleep_deprived","sex"), with=FALSE])

head(traje[[1]])

plot(traje)

# dynamic trajectory
trajdyn(traje[1])

testang.ltraj(traje[1], "relative")


# 
acfdist.ltraj(traje[1], "dist", lag=5)
acfang.ltraj(traje[1], lag=5)


# transform to data.frame and data.table
traje_df <- ld(traje)
traje_dt <- data.table(traje_df)
head(traje_dt)

ib2 <- set.limits(traje, begin="1960-01-01 00:00:00", dur= 1,
                  units="sec", pattern="%Y-%m-%d %H:%M:%S")

# data frame with some parameters
di <- sd2df(traje, "dist")


#PCA
traje_dt <- na.omit(traje_dt)
param_pca <- traje_dt[x <0.68,4:10, with=FALSE]
traje.pca <- prcomp(param_pca, center = TRUE, scale =TRUE)
print(traje.pca)
summary(traje.pca)
condition <-  traje_dt[x <0.68,sleep_deprived]
qplot(PC1, PC2, data = fortify(traje.pca), color = condition)
ggbiplot(traje.pca,choices=c(1,4), var.scale = 1)


#LDA
library(MASS)

# Standardize numeric variables and apply LDA
traje_z <- lapply(traje_dt, function(x) if (is.numeric(x)) scale(x) else x)
m <- lda(sleep_deprived ~ ., data = traje_z)
df <- fortify(m, traje_z)

g <- ggplot(df, aes(x = LD1, y = LD2)) +
  geom_point(aes(color = sleep_deprived)) + 
  stat_ellipse(aes(group =sleep_deprived , color = sleep_deprived)) +
  geom_axis(data = attr(df, "basis"), aes(label = abbreviate(.name))) + 
  ylim(-4, 4) + coord_equal()

print(g)


library(rgl)
plot3d(test_dt$PC1, test_dt$PC2, test_dt$PC3)






