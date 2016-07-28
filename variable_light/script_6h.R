library(rethomics)
setwd("/data/Diana/analyze/variable_light")
q <- fread("./query_6h.csv")
q <- buildEthoscopeQuery("/data/Diana/data_node/ethoscope_results", q)
q
 dt <- loadEthoscopeData(q[status=='ok'], FUN=sleepAnnotation, reference_hour = 9)

ethogramPlot(asleep, dt[t > days(0) & t < days(8)], condition=sex, time_unit_conversion = hours, error_bar = 'sem') +  annotate("rect", xmin=c(0, 24, 48, 72, 96, 120, 144, 168), xmax=c(6, 30, 54, 79, 102, 126, 150, 174), ymin=0, ymax=Inf, alpha=0.2, fill="yellow") 

overviewPlot(moving, dt[t > days(0) & t < days(8)], condition = sex) +  annotate("rect", xmin=c(0, 1, 2, 3, 4, 5, 6, 7), xmax=c(0.25, 1.25, 2.25, 3.25, 4.25, 5.25, 6.25, 7.25), ymin=0, ymax=Inf, alpha=0.2, fill="yellow") 
?makeLDAnnotation


ts_dt <-  dt[t > days(0) & t < days(8), .(avg_mov = mean(moving)), by="t,sex"]

ts_dt[, t_round := mins(5) * floor(t / (mins(5)))]
ts_dt <-  ts_dt[ , .(avg_mov = mean(avg_mov)), by="t_round,sex"]


pdf()
acf_res <- acf(ts_dt[sex=="F", avg_mov], lag.max = 500)
abline(v=288)

ggplot(ts_dt[sex=="F"], aes(t,avg_mov)) + geom_line()
dev.off()
getwd()


stat_dt <- dt[,.(sleep_total = mean(asleep[between(t,days(0), days(8))])),by=key(dt)]
