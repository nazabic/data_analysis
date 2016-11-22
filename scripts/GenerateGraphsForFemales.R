#Generate plots

#Covered distance
myTheme <-  theme(axis.text = element_text(size=21, face = "bold"), axis.title = element_text(size=21, face = "bold"), legend.text = element_text(size=17), legend.title = element_text(size=18), plot.title = element_text(size = 23, face = "bold"))
c1 <- ggplot(interesting_vars[sex=='female'], aes(y=covered_dist_cm, x=sleep_deprived)) +  geom_boxplot() + xlab("") + ylab("Trajectory length (cm)") + myTheme
c2 <- ggplot(interesting_vars[sex=='female'], aes(covered_dist_cm, fill=sleep_deprived)) + geom_density(alpha=0.5) + xlab("Trajectory length (cm)") 
c3<- ggplot(interesting_vars[sex=='female'],aes(covered_dist_cm,fill=sleep_deprived)) + geom_histogram(alpha=.5, position="identity", breaks=seq(0, 80 , by = 3)) + xlab("Trajectory length (cm)") 


# Total time spent in maze
t1 <- ggplot(interesting_vars[sex=='female'], aes(y=delta_t, x=sleep_deprived)) + geom_boxplot() + xlab("") + ylab("Total time spent in the maze (s)") + myTheme
t2 <- ggplot(interesting_vars[sex=='female'], aes(delta_t, fill=sleep_deprived)) + geom_density(alpha=0.5) + xlab("Total time spent in the maze (s)")
t3<- ggplot(interesting_vars[sex=='female'],aes(delta_t,fill=sleep_deprived)) + geom_histogram(alpha=.5, position="identity", breaks=seq(0, 250 , by = 10phi1)) + xlab("Total time spent in the maze (s)") 
wilcox.test(interesting_vars[sex=='female' & sleep_deprived=="sleep_deprived"]$delta_t, interesting_vars[sleep_deprived=="control" & sex=='female']$delta_t, paired=T)


#Velocity
t1 <- ggplot(interesting_vars[sex=='female'], aes(y=velocity*151,7326, x=sleep_deprived)) + geom_boxplot() + xlab("Sleep deprivation") + ylab("Mean velocity (mm/s)")
wilcox.test(interesting_vars[sex=='female' & sleep_deprived=="sleep_deprived"]$velocity, interesting_vars[sleep_deprived=="control" & sex=='female']$velocity, paired=T)
                                                                                      
# Exploratory score
es1 <- ggplot(interesting_vars[sex=='female'], aes(y=exploratoryScore, x=sleep_deprived)) + geom_boxplot() + xlab("Sleep deprivation") + ylab("Exploratory score")
es2 <- ggplot(interesting_vars[sex=='female'], aes(exploratoryScore, fill=sleep_deprived)) + geom_density(alpha=0.5) + xlab("Exploratory score")
es3 <- ggplot(interesting_vars[sex=='female'],aes(exploratoryScore,fill=sleep_deprived)) + geom_histogram(alpha=.5, position="identity", breaks=seq(0, 0.3 , by=0.01)) + xlab("Total time spent in the maze (s)") 

# Angle
phi1 <- ggplot(dt[sex=='female'], aes(y=phi, x=sleep_deprived)) + geom_boxplot() + xlab("Sleep deprivation") + ylab("Angle distribution")
phi2 <- ggplot(dt[sex=='female'], aes(phi, fill=sleep_deprived)) + geom_density(alpha=0.5) + xlab("Angle distrbution")
phi3 <- ggplot(dt[sex=='female'],aes(phi,fill=sleep_deprived)) + geom_histogram(alpha=.5, position="identity", breaks=seq(0, 180 , by=5)) + xlab("Angles distributions")

