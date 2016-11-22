library(rethomics)
setwd("/home/diana/github/sleep_analysis_experiments/20151106_dyn_sleep_pilot_nobug")
q <- fread("./query2.csv")
q[,sdi_f := sprintf("%02d",sdi)]
q[,interval := round(((11-sdi) ^ 1.7)) * 20]
q <- buildEthoscopeQuery("/mnt/ethoscope_results", q)

maxVelocityClassifierMasked <- function(data,velocity_threshold=.006, delay=6){
  d <- copy(data)
  d[,dt := c(NA,diff(t))]
  d[,max_velocity := 10^(xy_dist_log10x1000/1000)/dt ]
  d[,interaction_id := cumsum(has_interacted)]
  d[,
    masked := t < (t[1] + delay),
    by=interaction_id]
  
  
  d[,beam_cross := abs(c(0,diff(sign(.5 - x))))]
  d[,beam_cross := as.logical(beam_cross)]
  
  d[ ,max_velocity := ifelse(masked & interaction_id != 0, 0, max_velocity)]
  d[,beam_cross := ifelse(masked & beam_cross, FALSE, beam_cross)]
  d[,interaction_id := NULL]
  d[,masked := NULL]
  
  d_small <- d[,.(
    has_interacted = any(has_interacted),
    max_velocity = max(max_velocity),
    beam_cross = any(beam_cross)
  ), by="t_round"]
  
  d_small[, moving :=  ifelse(max_velocity > velocity_threshold, TRUE,FALSE)]
  #d_small[, moving :=  beam_cross]
  #print(d_small)
  d_small
}




dt <- loadEthoscopeData(q[status=="OK" & sdi==0],reference_hour = 9,
                              FUN=sleepAnnotation,
                              motion_classifier_FUN = maxVelocityClassifierMasked)


dt[, t:= t-days(baseline_days)]

# create a column that contains the hour of the day 
dt[, hour:= (t %% 86400)/3600]

# select all the data that is before sleep deprivation ( t< 172800) and between specifc hours ( noon to 16 in the afternoon) from baseline
bdt <- boutAnalysis(moving, dt[hour > 11 & hour < 17 & t < days(1)  & t > days(0)])


# remove first row in order to avoid the special case where the length of the first bout can be 0
bdt <- bdt[, .SD[-1,], by = key(dt)]



# bin using the rounded log
bdt[, log_length := log10(length)]

bdt[,round_log_length := round(log_length,2)]
summary_bout_dt <- bdt[moving == F & length < 300][,.(number=.N),by=c(key(bdt),"round_log_length")]

#compute the relative frequency of each bout duration
summary_bout_dt[, frequency := number/sum(number)]
summary_bout_dt[, log_frequency := log10(frequency)]


lm_eqn <- function(df){
  m <- lm(log_frequency ~ round_log_length, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 2)))
  as.character(as.expression(eq));                 
}


#pl <- ggplot(dt, aes(x = max_velocity,col=sex)) + geom_density()

# subdt <- summary_bout_dt[region_id == 19 & round_log_length < 4 & round_log_length > 1]
# mod <- lm(log_frequency ~ round_log_length, subdt)
# pdf("/tmp/test.pdf")
# plot(log_frequency ~ round_log_length, subdt)
# dev.off()
# summary(mod)

eqns <- by(summary_bout_dt, summary_bout_dt$region_id, lm_eqn)
dt_eqn <- data.frame(eq = unclass(eqns), region_id = as.numeric(names(eqns)))

# inactivity bouts of 3~4h from noon in the afternoon and using the rounded log of the length of a bout, for each fly
pl <- ggplot(summary_bout_dt, aes(y = log_frequency, x = round_log_length, col=sex))  + geom_point()  + facet_wrap( ~ region_id) + geom_smooth(metho="lm")

pl + geom_text(data = dt_eqn, aes(x = 2, y = 3.5, label = eq, family = "serif"), 
               color = 'blue',  parse = TRUE)


#for all flies
pl <- ggplot(summary_bout_dt, aes(y = frequency, x = round_log_length, col=sex))  + scale_y_log10() + geom_point()  + geom_smooth(metho="lm")


# distribution of maximum velocity in the INACTIVE bouts for each fly of one experiment
ggplot(dt[experiment_id == '2015-11-15_18-33-41_015aeeee10184bb39b0754e75cef7900.db' & max_velocity < 0.004], aes(max_velocity, col=sex)) + geom_histogram() + facet_wrap( ~ region_id)

