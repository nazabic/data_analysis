library(rethomics)
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



motionAnnotation <- function (data, time_window_length = 5, motion_classifier_FUN = maxVelocityClassifier, ...) 
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



dt <- loadEthoscopeData(q[status=="OK"],reference_hour = 9,
                        FUN=motionAnnotation,
                        motion_classifier_FUN = maxVelocityClassifierMasked, velocity_threshold = 0.006)


dt[, t:= t-days(baseline_days)]

# create a column that contains the hour of the day 
dt[, hour:= (t %% 86400)/3600]

# select all the data that is before sleep deprivation ( t< 172800) and between specifc hours ( noon to 16 in the afternoon) from baseline
bdt_all <- boutAnalysis(moving, dt)

bdt <- boutAnalysis(moving, dt[ hour > 12 & hour < 18 & sex == 'M'])

# remove first row in order to avoid the special case where the length of the first bout can be 0
bdt <- bdt[moving == F & length < 300, .SD[-1,], by = key(dt)]

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


