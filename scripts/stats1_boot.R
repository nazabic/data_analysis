b.stat <- function(data, num, stat) {
  resamples <- lapply(1:num, function(i) sample(data, length(data), replace=T))
  r.stat <- sapply(resamples, stat)
  std.err <- sqrt(var(r.stat))
  list(std.err=std.err, resamples=resamples, stats=r.stat)   
}


sd_fem_bstat <-  b.stat(interesting_vars_right[sex == "female" & sleep_deprived == "sleep_deprived"]$velocity, 1000, median)
nsd_fem_bstat <- b.stat(interesting_vars_right[sex == "female" & sleep_deprived == "control"]$velocity, 1000, median)

hist(sd_fem_bstat$stats, col=rgb(1,0,0,0.5), xlab ='')
hist(nsd_fem_bstat$stats, col=rgb(0,0,1,0.5), add=T)
mdt <- rbind(
     data.table(es=sd_fem_bstat$stats,sd="sleep deprived"),
     data.table(es=nsd_fem_bstat$stats,sd="control")
)

mdt
ggplot(mdt,aes(es,fill=sd)) + geom_histogram(alpha=.5, position="identity") 
box()








 
library(boot)

b <- boot(dt[sleep_deprived==TRUE & sex=='female']$covered_dist, function(u,i) mean(u[i]), R = 1000)
boot.ci(b, type = c("norm", "basic", "perc"))
nsd_fem_bstat <- b.stat(female_vars[sleep_deprived==TRUE & sex =='female']$covered_dist, 10000, mean)
cdf <- ecdf(nsd_fem_bstat)
plot(cdf)

library(stats)


########First part of the maze
#distance
wilcox.test(interesting_vars[sex == "female" & sleep_deprived=="sleep_deprived"]$covered_dist, interesting_vars[sex =="female" & sleep_deprived=="control"]$covered_dist)
wilcox.test(interesting_vars[sex == "male" & sleep_deprived=="sleep_deprived"]$covered_dist, interesting_vars[sex =="male" & sleep_deprived=="control"]$covered_dist)

#velocity
wilcox.test(interesting_vars[sex == "female" & sleep_deprived=="sleep_deprived"]$velocity, interesting_vars[sex =="female" & sleep_deprived=="control"]$velocity, alternative="less")
wilcox.test(interesting_vars[sex == "male" & sleep_deprived=="sleep_deprived"]$velocity, interesting_vars[sex =="male" & sleep_deprived=="control"]$velocity)

#time
wilcox.test(interesting_vars[sex == "female" & sleep_deprived=="sleep_deprived"]$time_finish, interesting_vars[sex =="female" & sleep_deprived=="control"]$time_finish)
wilcox.test(interesting_vars[sex == "male" & sleep_deprived=="sleep_deprived"]$time_finish, interesting_vars[sex =="male" & sleep_deprived=="control"]$time_finish)

########Second part of the maze

#velocity
wilcox.test(interesting_vars_right[sleep_deprived=="sleep_deprived"]$velocity, interesting_vars_right[sleep_deprived=="control"]$velocity)

wilcox.test(interesting_vars[sleep_deprived=="sleep_deprived"]$velocity, interesting_vars[sleep_deprived=="control"]$velocity)



#distance
t.test(interesting_vars[sex == "female" & sleep_deprived=="sleep_deprived"]$covered_dist, interesting_vars[sex =="female" & sleep_deprived=="control"]$covered_dist)
t.test(interesting_vars[sex == "male" & sleep_deprived=="sleep_deprived"]$covered_dist, interesting_vars[sex =="male" & sleep_deprived=="control"]$covered_dist)

#velocity
t.test(interesting_vars[sex == "female" & sleep_deprived=="sleep_deprived"]$velocity, interesting_vars[sex =="female" & sleep_deprived=="control"]$velocity)
t.test(interesting_vars[sex == "male" & sleep_deprived=="sleep_deprived"]$velocity, interesting_vars[sex =="male" & sleep_deprived=="control"]$velocity)

#time
t.test(interesting_vars[sex == "female" & sleep_deprived=="sleep_deprived"]$time_finish, interesting_vars[sex =="female" & sleep_deprived=="control"]$time_finish)
t.test(interesting_vars[sex == "male" & sleep_deprived=="sleep_deprived"]$time_finish, interesting_vars[sex =="male" & sleep_deprived=="control"]$time_finish)
