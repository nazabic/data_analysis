library(rethomics)
setwd("/home/diana/github/sleep_analysis_experiments/dd_analysis")
q1 <- fread("./query.csv")
q <- buildEthoscopeQuery("/data/ethoscope_results", q1)

dt <- loadEthoscopeData(q[status!='deadpupa'],reference_hour = 9,
                        FUN=sleepAnnotation, 
                        cache_files = F)
dt_fm <- dt[sex!='U']
ethogramPlot(asleep, dt_fm, condition = sex,facet_var = genotype)

overviewPlot(asleep, dt)

