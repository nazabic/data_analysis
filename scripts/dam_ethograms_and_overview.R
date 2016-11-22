DAILY_DATA_DIR <- "/home/diana/Desktop/dailyData"

# EXPERIMENT 4

query = data.table(start_date="2016-04-17", stop_date = "2016-04-22", machine_id=c("M003", "M004"), 
                   treatment = c("sleep_deprived", "control")
)
print(query)

dt <- loadDailyDAM2Data(DAILY_DATA_DIR, query,reference_hour = 10.0, FUN=sleepDAMAnnotation)
dt <- dt[,curateDeadAnimals(.SD,hours(15)),by=key(dt)]
myTheme <-  theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), axis.text.x = element_text(size=20), axis.text.y = element_text(size=5), legend.text = element_text(size=20), legend.title = element_text(size=20))

ethogramPlot(asleep,dt,treatment,error_bar = "sem")
overviewPlot(moving,dt,treatment) + myTheme



# EXPERIMENT 5


query = data.table(start_date="2016-04-28", stop_date = "2016-05-02", machine_id=c("M001", "M002"), 
                   treatment = c("sleep_deprived", "control")
)
print(query)

dt <- loadDailyDAM2Data(DAILY_DATA_DIR, query,reference_hour = 10.0, FUN=sleepDAMAnnotation)
dt <- dt[,curateDeadAnimals(.SD,hours(72)),by=key(dt)]
myTheme <-  theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), axis.text.x = element_text(size=20), axis.text.y = element_text(size=5), legend.text = element_text(size=20), legend.title = element_text(size=20))

ethogramPlot(asleep,dt,treatment,error_bar = "sem")
overviewPlot(moving,dt,treatment) + myTheme

# EXPERIMENT 6


query = data.table(start_date="2016-05-01", stop_date = "2016-05-06", machine_id=c("M003", "M004"), 
                   treatment = c("sleep_deprived", "control")
)
print(query)


dt <- loadDailyDAM2Data(DAILY_DATA_DIR, query,reference_hour = 10.0, FUN=sleepDAMAnnotation)
dt <- dt[,curateDeadAnimals(.SD,hours(48)),by=key(dt)]
myTheme <-  theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), axis.text.x = element_text(size=20), axis.text.y = element_text(size=5), legend.text = element_text(size=20), legend.title = element_text(size=20))

ethogramPlot(asleep,dt,treatment,error_bar = "sem")
overviewPlot(moving,dt,treatment) + myTheme

# EXPERIMENT 7


query = data.table(start_date="2016-05-08", stop_date = "2016-05-13", machine_id=c("M001", "M002"), 
                   treatment = c("sleep_deprived", "control")
)
print(query)


dt <- loadDailyDAM2Data(DAILY_DATA_DIR, query,reference_hour = 10.0, FUN=sleepDAMAnnotation)
dt <- dt[,curateDeadAnimals(.SD,hours(48)),by=key(dt)]
myTheme <-  theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), axis.text.x = element_text(size=20), axis.text.y = element_text(size=5), legend.text = element_text(size=20), legend.title = element_text(size=20))

ethogramPlot(asleep,dt,treatment,error_bar = "sem")
overviewPlot(moving,dt,treatment) + myTheme


# EXPERIMENT 8


query = data.table(start_date="2016-05-14", stop_date = "2016-05-19", machine_id=c("M002", "M001"), 
                   treatment = c("sleep_deprived", "control")
)
print(query)


dt <- loadDailyDAM2Data(DAILY_DATA_DIR, query,reference_hour = 10.0, FUN=sleepDAMAnnotation)
dt <- dt[,curateDeadAnimals(.SD,hours(90)),by=key(dt)]
myTheme <-  theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), axis.text.x = element_text(size=20), axis.text.y = element_text(size=5), legend.text = element_text(size=20), legend.title = element_text(size=20))

ethogramPlot(asleep,dt,treatment,error_bar = "sem")
overviewPlot(moving,dt,treatment) + myTheme


# EXPERIMENT 9


query = data.table(start_date="2016-05-15", stop_date = "2016-05-20", machine_id=c("M003", "M004"), 
                   treatment = c("sleep_deprived", "control")
)
print(query)


dt <- loadDailyDAM2Data(DAILY_DATA_DIR, query,reference_hour = 10.0, FUN=sleepDAMAnnotation)
dt <- dt[,curateDeadAnimals(.SD,hours(90)),by=key(dt)]
myTheme <-  theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), axis.text.x = element_text(size=20), axis.text.y = element_text(size=5), legend.text = element_text(size=20), legend.title = element_text(size=20))

ethogramPlot(asleep,dt,treatment,error_bar = "sem")
overviewPlot(moving,dt,treatment) + myTheme


