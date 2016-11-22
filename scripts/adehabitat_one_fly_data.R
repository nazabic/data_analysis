library(rethomics)
library(tools)
library(adehabitatLT)

setwd("/home/diana/github/sleep_analysis_experiments/20151106_dyn_sleep_pilot_nobug")
q <- fread("./query2.csv")
q[,sdi_f := sprintf("%02d",sdi)]
q[,interval := round(((11-sdi) ^ 1.7)) * 20]
q <- buildEthoscopeQuery("/mnt/ethoscope_results", q)


dt <- loadEthoscopeData(q[status=="OK"],reference_hour = 9,
                        FUN=sleepAnnotation, 
                        cache_files = F,
                        motion_classifier_FUN = maxVelocityClassifierMasked, velocity_threshold = 0.006)
myfly <-  dt[region_id == 1]

myfly[, hour:= (t %% 86400)/3600]

# be able to have miliseconds
options(digits.secs = 3)


# time as POSIXct for adehabitat
time <- as.POSIXct(myfly[, t], origin = "1960-01-01")
traje <-as.ltraj(myfly[, c("x","y"), with=FALSE], time, myfly[, experiment_id], infolocs = myfly[,c("asleep","sex"), with=FALSE])

head(traje[[1]])

trajdyn(traje[1])

testang.ltraj(traje[1], "relative")
