#Bootstrapping

b.stat <- function(data, num, stat) {
        resamples <- lapply(1:num, function(i) sample(data, length(data), replace=T))
        r.stat <- sapply(resamples, stat)
        std.err <- sqrt(var(r.stat))
        list(std.err=std.err, resamples=resamples, stats=r.stat)   
}


diff_dist <- interesting_vars[sex == "female" & sleep_deprived == TRUE]$velocity - interesting_vars[sex == "female" & sleep_deprived == FALSE]$velocity

b <- boot(female_vars[sleep_deprived==TRUE]$covered_dist, function(u,i) mean(u[i]), R = 1000)
boot.ci(boot.out = b, type = c("norm", "basic", "perc"))
