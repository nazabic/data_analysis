# is the temporal pattern of the fly following a fractal ?

rm(list=ls())

library(rethomics)
library(ggplot2)

setwd("/home/diana/github/sleep_analysis_experiments/20151106_dyn_sleep_pilot_nobug")
q <- fread("./query2.csv")
q[,sdi_f := sprintf("%02d",sdi)]
q[,interval := round(((11-sdi) ^ 1.7)) * 20]
q <- buildEthoscopeQuery("/mnt/ethoscope_results", q)



curateSparseRoiData <- function (data, window = 60, min_points = 20) 
{
  d <- copy(data)
  d[, `:=`(t_w, window * floor(t/window))]
  sparsity <- d[, `:=`(t_w, window * floor(t/window))]
  d[, `:=`(sparsity, .N), by = t_w]
  d <- d[sparsity > min_points, ]
  d$t_w <- NULL
  d$sparsity <- NULL
  d
}

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



motionAnnotation <- function (data, time_window_length = 1, motion_classifier_FUN = maxVelocityClassifier, ...) 
{
  d <- copy(data)
  ori_keys <- key(d)
  d <- curateSparseRoiData(d)
  if (nrow(d) < 1) 
    return(NULL)
  d[, `:=`(t_round, time_window_length * floor(d[, t]/time_window_length))]
  setkeyv(d, "t_round")
  d_small <- motion_classifier_FUN(d, ...)
  if (key(d_small) != "t_round") 
    stop("Key in output of motion_classifier_FUN MUST be `t_round'")
  setnames(d_small, "t_round", "t")
  d$t <- NULL
  d_small <- d_small[unique(d)]
  t_out <- seq(from = d_small[1, t], to = d_small[.N, t], 
               by = time_window_length)
  time_map <- data.table(t = t_out, key = "t")
  missing_val <- time_map[!d_small]
  d_small <- d_small[time_map, roll = T]
  d_small[, `:=`(is_interpolated, FALSE)]
  d_small[missing_val, `:=`(is_interpolated, TRUE)]
  d_small[is_interpolated == T, `:=`(moving, FALSE)]
  setkeyv(d_small, ori_keys)
  na.omit(d_small)
}

# the velocity threshold coresponds to 4 mm/s

dt <- loadEthoscopeData(q[status=="OK"],reference_hour = 9,
                        FUN=motionAnnotation, 
                        cache_files=T,
                        motion_classifier_FUN = maxVelocityClassifier, velocity_threshold = 0.010)


dt_sleep <- loadEthoscopeData(q[status=="OK"],reference_hour = 9,
                              FUN=sleepAnnotation, 
                              cache_files = F,
                              motion_classifier_FUN = maxVelocityClassifierMasked)

dt[, t:= t-days(baseline_days)]

dt[, hour:= (t %% 86400)/3600]

bdt <- boutAnalysis(moving,dt[hour > 9 & hour < 14])

# remove first row in order to avoid the special case where the length of the first bout can be 0
bdt <- bdt[, .SD[-1,], by = key(dt)]

all_bouts_for_each_animal <- bdt[start_time > days(-1) & start_time < days(-0.5)]
summary_bout_dt <- bdt[start_time > days(-1) & start_time < days(-0.5)][,.(number=.N, median_length=median(length)),by=c(key(bdt),"moving")]
ggplot(summary_bout_dt[sex=='M'], aes(number, median_length)) + geom_point(size=4)


# one fly case
one_fly_bdt <- bdt[start_time > days(-2) & start_time < days(0) & region_id == 1 & sex == 'F']
ggplot(one_fly_bdt[moving == F & length >100], aes(length)) + geom_histogram(binwidth=1)
long_moving_bouts <- one_fly_bdt[length > 50][,.(count_long_bouts=.N), by = c(key(bdt), "moving")]


# second 2 days of baseline (we don't consider first day)
bdt <- bdt[start_time > days(-2) & start_time < days(0)]

# the distribution of moving bout lengths for one fly
ggplot(bdt[moving ==T], aes(length)) + geom_histogram(binwidth=1) + facet_wrap(sex ~ region_id)

# the distribution of NOT moving bouts lengths for one fly 
ggplot(bdt[moving ==F & length <100], aes(length)) + geom_histogram(binwidth=1) + facet_wrap(sex ~ region_id)


# count how many short moving bouts (ex. less than 5 s) are for each fly
short_moving_bouts <- bdt[length < 5][,.(count_short_bouts=.N), by = c(key(bdt), "moving")]
ggplot(short_moving_bouts[moving == F], aes(y=count_short_bouts, x=sex)) + geom_boxplot()
ggplot(short_moving_bouts[moving == T], aes(y=count_short_bouts, x=sex)) + geom_boxplot()


# count how many long moving bouts (ex. more than 50 s) are for each fly
short_moving_bouts <- bdt[length > 70][,.(count_long_bouts=.N), by = c(key(bdt), "moving")]
ggplot(short_moving_bouts[moving == T], aes(y=count_long_bouts, x=sex)) + geom_boxplot()


# log-log plots

bdt[,log_length := log10(length)]
bdt[,round_log_length := round(log_length,2)]
bdt[, day:= floor(start_time/86400)]



# slope of all flies
summary_bout_dt <- bdt[moving == F & length < 50 & day == 0,.(number=.N),by=c(key(bdt),"round_log_length")]

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

eqns <- by(summary_bout_dt, summary_bout_dt$region_id, lm_eqn)
dt_eqn <- data.frame(eq = unclass(eqns), region_id = as.numeric(names(eqns)))

pl <- ggplot(summary_bout_dt, aes(y = log_frequency, x = round_log_length, col=sex))  + geom_point()  + facet_wrap( ~ region_id) + geom_smooth(metho="lm")

pl + geom_text(data = dt_eqn, aes(x = 1.2, y = 1, label = eq, family = "serif"), 
               color = 'blue',  parse = TRUE)




# get the slope and intercept into a data table
dt[, t:= t-days(baseline_days)]

dt[, hour:= (t %% 86400)/3600]

bdt <- boutAnalysis(moving,dt)

# remove first row in order to avoid the special case where the length of the first bout can be 0
bdt <- bdt[, .SD[-1,], by = key(dt)]

bdt[,log_length := log10(length)]
bdt[,round_log_length := round(log_length,2)]
bdt[, day:= floor(start_time/86400)]

lm_eqn <- function(df){
  m <- lm(log_frequency ~ round_log_length, df);
  out <- as.numeric(c(format(coef(m)[1], digits = 2), 
                      format(coef(m)[2], digits = 2), 
                      format(summary(m)$r.squared, digits = 2)))
  names(out) <- c("intercept", "slope", "r_squared")
  return(out)
}

different_lengths <- seq(20, 500, 20)

get_rsquared_mean <- function(l, bdt){
  summary_bout_dt <- bdt[moving == F & length < l ,.(number=.N),by=c(key(bdt),"round_log_length")]
  summary_bout_dt[, frequency := number/sum(number)]
  summary_bout_dt[, log_frequency := log10(frequency)]
  
  eqns <- by(summary_bout_dt, summary_bout_dt$region_id, lm_eqn)
  dt_eqn <- data.table(do.call(rbind, eqns))
  dt_eqn[, region_id := as.numeric(names(eqns))] 
  mean(dt_eqn$r_squared)
}


means <- do.call(get_rsquared_mean, c(differnt_lengths, bdt))



# slope of a fly per day
summary_bout_dt <- bdt[region_id == 1 & moving == F & length < 300 & day %in% c(1, 2),.(number=.N),by=c(key(bdt),"round_log_length", "day")]

summary_bout_dt[, frequency := number/sum(number)]
summary_bout_dt[, log_frequency := log10(frequency)]

eqns <- by(summary_bout_dt, summary_bout_dt$day, lm_eqn)
dt_eqn <- data.frame(eq = unclass(eqns), day = as.numeric(names(eqns)))

pl <- ggplot(summary_bout_dt, aes(y = log_frequency, x = round_log_length, col=sex))  + geom_point()  + facet_wrap( ~ day) + geom_smooth(metho="lm")

pl + geom_text(data = dt_eqn, aes(x = 1, y = 1, label = eq, family = "serif"), 
               color = 'blue',  parse = TRUE)


# how does the average correlation of the slopes vary with the length used for inactivity (before sleep) 



