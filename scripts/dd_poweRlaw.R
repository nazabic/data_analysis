library(rethomics)
library(poweRlaw)
setwd("/home/diana/github/sleep_analysis_experiments/dd_analysis")
q1 <- fread("./query.csv")
q <- buildEthoscopeQuery("/mnt/ethoscope_results", q1)

dt <- loadEthoscopeData(q[status!='deadpupa'],reference_hour = 9,
                        FUN=sleepAnnotation, 
                        cache_files = F)
dt_fm <- dt[sex!='U']

ethogramPlot(asleep, dt_fm, condition = sex,facet_var = genotype)

overviewPlot(asleep, dt_fm[t < days(6)])


#dt_fm[, t:= t-days(baseline_days)]

# create a column that contains the hour of the day 
dt_fm[, hour:= (t %% 86400)/3600]

# select all the data that is before sleep deprivation ( t< 172800) and between specifc hours ( noon to 16 in the afternoon) from baseline
bdt <- boutAnalysis(moving, dt_fm[hour > 12 & hour < 18 & machine_name=='ETHOSCOPE_001'])

# remove first row in order to avoid the special case where the length of the first bout can be 0
bdt <- bdt[, .SD[-1,], by = key(dt_fm)]

#restart graphics
dev.off()
plot.new()

# the power law distribution is defined by the xmin and alpha (exponent of the power law also called pars)
m_m=displ$new(bdt$length)

est=estimate_xmin(m_m)
m_m$setXmin(est)

est=estimate_pars(m_m)
m_m$setPars(est)


plot(m_m)

lines(m_m,col=2)

bs = bootstrap(m_m, no_of_sims = 100, threads = 4)

hist(bs$bootstraps[,2], breaks="fd")
hist(bs$bootstraps[,3], breaks="fd")

plot(jitter(bs$bootstraps[,2], factor=1.2), bs$bootstraps[,3])

bs_p = bootstrap_p(m_m, no_of_sims = 100, threads = 4)

bs_p
