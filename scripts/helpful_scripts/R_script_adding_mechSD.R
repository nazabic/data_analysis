install.packages("devtools")
library(devtools)
setwd("C:")
.libPaths("C:/Program Files/R/R-3.2.0/library")
install_github("gilestrolab/rethomics",subdir = "rethomics")
library(rethomics)

#First of all, clean the enviroment and load the library
rm(list = ls(all = TRUE)) 
library(rethomics)

# change me!!!! This is the placement of the real data
DAILY_DATA_DIR <- "N:/sleepData/dailyData"

QUERY_PATH <- "C:/Users/ebeckwit/Dropbox/esteban/gg lab/sleep behavior experiments/merger of experiments/2015_04_29 + 2015_05_03 - social sd between zt12 and zt24/Adding mechSD from 2015_05_17/query.csv"

query <- fread(QUERY_PATH)

print(query)

#this is the magic moment
dt <- fetchDAMData(DAILY_DATA_DIR, query,reference_hour = 10.0, tz = "BST", FUN=sleepDAMAnnotation)

#these two make the graph for the general visualization of the data. The ethogramPlot has the trick to plot parcial data
ethogramPlot(asleep,dt,treatment,error_bar = "sem")
overviewPlot(moving,dt,treatment,normalise_var_per_id = F)
ethogramPlot(asleep,dt[t > days(2) & t<days(4) + hours(24)],treatment,error_bar = "sem")

#to generate a new data set that is a subset of the original. And then you have the plot and the visualization
dt_rebound_24h <- dt[t > days(4.0) & t < (days(4.0)+hours(24)),]
pl <- ethogramPlot(asleep,dt_rebound_24,treatment,error_bar="sem") #this makes a facet by experiment
print(pl)

#to generate a new data set that is a subset of the original, in this case 3h window
dt_rebound_12h <- dt[t > days(4.0) & t < (days(4.0)+hours(12)),]
pl12hmean <- ethogramPlot(asleep,dt_rebound_12h,treatment,error_bar="sem")
print(pl12hmean)

#to generate a new data set that is a subset of the original, in this case 3h window
dt_rebound_3h <- dt[t > days(4.0) & t < (days(4.0)+hours(3)),]
pl3hmean <- ethogramPlot(asleep,dt_rebound_3h,treatment,error_bar="sem")
print(pl3hmean)

#this generates a BOXPLOTS of the selected datase, could be the entire dt or the subsets. 
dt_sleep_fract_in_rebound_24= dt_rebound_24h[,.(sleep_fraction=mean(asleep)),by=c("treatment","exp",key(dt_rebound_24h))]
#for the mean 
pl_boxplotmean_24 <- ggplot(dt_sleep_fract_in_rebound_24,aes(treatment, sleep_fraction,fill=treatment)) + geom_boxplot(notch=FALSE)
pl_boxplotmean_24

dt_sleep_fract_in_rebound_12 = dt_rebound_12h[,.(sleep_fraction=mean(asleep)),by=c("treatment","exp",key(dt_rebound_12h))]
#for the mean 
pl_boxplotmean_12 <- ggplot(dt_sleep_fract_in_rebound_12,aes(treatment, sleep_fraction,fill=treatment)) + geom_boxplot(notch=FALSE)
pl_boxplotmean_12

dt_sleep_fract_in_rebound_3 = dt_rebound_3h[,.(sleep_fraction=mean(asleep)),by=c("treatment","exp",key(dt_rebound_3h))]
#for the mean 
pl_boxplotmean_3 <- ggplot(dt_sleep_fract_in_rebound_3,aes(treatment, sleep_fraction,fill=treatment)) + geom_boxplot(notch=FALSE)
pl_boxplotmean_3

install.packages("betareg")
library("betareg") 

sink(file="stats.txt") #check your working directory because that's where this will go!
#!!!!!!!!!!!CHECK IF CORRECT!!!!!!!!!!
#Statitistics!!!with a linear model. - THE ASSUMPTIONS ARE NOT TESTED!!!
model <- lm(sleep_fraction ~ treatment, dt_sleep_fract_in_rebound_3)
print(summary(model))# show results
#write(capture.output(summary(model)), "lm.txt") # generets a .txt with the output of the statistical anlisis

#statistics with an anova (again, not sure how wrong this is). - THE ASSUMPTIONS ARE NOT TESTED!!!
ANOVA <- aov(sleep_fraction ~ treatment, data=dt_sleep_fract_in_rebound_3) 
summary(ANOVA) # show results
#write(capture.output(summary(ANOVA)), "ANOVA.txt") # generets a .txt with the output of the statistical anlisis

tukey<-TukeyHSD(ANOVA)
tukey
#write.csv(data.frame(tukey$`treatment:exp`), 'tukeyresults.txt') # generates a .txt with the tukey output. After the $ put your variable, could be the variables or the interaction

#betareg, this works when the data has ceiling or "flooring" deviations. There is a package needed, this is install in the first line.
mod <- betareg(sleep_fraction ~ treatment, data=dt_sleep_fract_in_rebound_3[sleep_fraction!=1 & sleep_fraction!=0])
summary(mod)

#Non parametric
wil <- print(pairwise.wilcox.test(dt_sleep_fract_in_rebound_3$sleep_fraction, dt_sleep_fract_in_rebound_3$treatment,p.adjust.method ="bonferroni"))

sink()

#bout lenght analysis - preliminar!!!!! Solo funciona por monitor, y no se porque me pierde el rotulo de tratamiento en el camino
#cuando hace el boutAnalysis usa el experiment_id, pero pierde la variable que yo introduzco, cuando tengo cosas que deberia
#promediar por variable las pierte.

bout_dt_12 <- boutAnalysis(asleep,dt_rebound_12h)
sleep_bout_dt_12 <- bout_dt_12[asleep==T]

summary_bout <- sleep_bout_dt[,
                              .(  n=.N, 
                                  median_length=median(length)),
                                  by=key(sleep_bout_dt)]

ggplot(summary_bout,aes(median_length,n, linestyle)) + 
  geom_point()


bouts_box_plot<- ggplot(sleep_bout_dt,aes(experiment_id, length,fill=experiment_id)) + geom_boxplot(notch=FALSE)
bouts_box_plot
