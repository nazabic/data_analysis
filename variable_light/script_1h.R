library(rethomics)
setwd("/data/Diana/analyze/variable_light")
q <- fread("./query_1h.csv")
q <- buildEthoscopeQuery("/data/Diana/data_node/ethoscope_results", q)
q
dt <- loadEthoscopeData(q[status=='ok'], FUN=sleepAnnotation, reference_hour = 9)

dt$light_duration <- factor(dt$light_duration)
ethogramPlot(asleep, dt[light_duration==6 & t > days(0) & t < days(8)], condition=sex, time_unit_conversion = hours, error_bar = 'sem') +  annotate("rect", xmin=c(0, 24, 48, 72, 96, 120, 144, 168), xmax=c(1, 25, 49, 73, 97, 121, 145, 169), ymin=0, ymax=Inf, alpha=0.2, fill="yellow") 

q
overviewPlot(moving, dt[t > days(0) & t < days(8)], condition = sex) +  annotate("rect", xmin=c(0, 1, 2, 3, 4, 5, 6, 7), xmax=c(0.042, 1.042, 2.042, 3.042, 4.042, 5.042, 6.042, 7.042), ymin=0, ymax=Inf, alpha=0.2, fill="yellow") 
