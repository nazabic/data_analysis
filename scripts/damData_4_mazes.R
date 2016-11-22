
rm(list=ls())
library(rethomics)

# change me!!!!
DAILY_DATA_DIR <- "/home/diana/Desktop/dailyData"

# sleep_deprived males Tuesday

query = data.table(start_date="2016-03-18", stop_date = "2016-03-19", machine_id=c("M009"), 
                                                                      treatment = c("sleep_deprived")
                                                                )
print(query)

dt <- loadDailyDAM2Data(DAILY_DATA_DIR, query,reference_hour = 10.0, FUN=sleepDAMAnnotation)

myTheme <-  theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), axis.text.x = element_text(size=20), axis.text.y = element_text(size=5), legend.text = element_text(size=20), legend.title = element_text(size=20))

ethogramPlot(asleep,dt,treatment,error_bar = "sem")
overviewPlot(moving,dt,treatment) + myTheme


# sleep_deprived females Wednesday

query = data.table(start_date="2015-08-07", stop_date = "2015-08-13", machine_id=c("M015", "M005"), 
                   treatment = c("control_f", "sleep_deprived_f")
)
print(query)

dt <- fetchDAMData(DAILY_DATA_DIR, query,reference_hour = 10.0, FUN=sleepDAMAnnotation)

ethogramPlot(asleep,dt[t > days(0) & t<days(5)],treatment,error_bar = "sem")
overviewPlot(moving,dt,treatment,normalise_var_per_id = F)




# sleep_deprived females Thursday

query = data.table(start_date="2015-08-07", stop_date = "2015-08-13", machine_id=c("M009", "M004"), 
                   treatment = c("control", "sleep_deprived")
)
print(query)

dt <- fetchDAMData(DAILY_DATA_DIR, query,reference_hour = 10.0, FUN=sleepDAMAnnotation)

ethogramPlot(asleep,dt[t > days(3) & t<days(6)],treatment,error_bar = "sem") 
overviewPlot(moving,dt[t > days(4) & t<days(6) & region_id < 24],treatment,normalise_var_per_id = F) 

