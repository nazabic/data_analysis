#First of all, clean the enviroment and load the library
rm(list = ls(all = TRUE)) 
library(rethomics)

# change me!!!! This is the placement of the real data
DAILY_DATA_DIR <- "/home/diana/Desktop/dailyData"

#I can define the query by extention
query = data.table(start_date=c("2015-06-15","2015-06-15","2015-06-15","2015-06-16","2015-06-16","2015-06-16","2015-06-30","2015-06-30","2015-06-30"),
                        stop_date= c("2015-06-20","2015-06-20","2015-06-20","2015-06-21","2015-06-21","2015-06-21","2015-07-05","2015-07-05","2015-07-05"), 
                        exp=c("A","A","A","B","B","B","C","C","C"),
                        machine_id=c("M006","M007","M008","M010","M011","M012","M001","M004","M005"), 
                        treatment=c("3-CS-female_M-tube","2-w-male_M-tube","1-mock","3-CS-female_M-tube","2-w-male_M-tube","1-mock","1-mock",
                                          "3-CS-female_M-tube",
                                    "2-w-male_M-tube"))


print(query)

#this is the magic moment
dt <- fetchDAMData(DAILY_DATA_DIR, query,reference_hour = 10.0, FUN=sleepDAMAnnotation)

#these two make the graph for the general visualization of the data. The ethogramPlot has the trick to plot parcial data
ethogramPlot(asleep,dt,treatment,error_bar = "sem")
overviewPlot(moving,dt,treatment,normalise_var_per_id = F)
overviewPlot(activity,dt,treatment,normalise_var_per_id = F)
overviewPlot(asleep,dt,treatment,normalise_var_per_id = F)
ethogramPlot(asleep,dt[t > days(4) & t<days(4) + hours(24)],treatment,error_bar = "sem")

#to generate a new data set that is a subset of the original. And then you have the plot and the visualization
dt_rebound <- dt[t > days(4.0) & t < (days(4.0)+hours(24)),]
pl <- ethogramPlot(asleep,dt_rebound,treatment,exp,error_bar="sem") #this makes a facet by experiment
print(pl)
plmean <- ethogramPlot(asleep,dt_rebound,treatment,error_bar="sem")
print(plmean)

#to generate a new data set that is a subset of the original, in this case 3h window
dt_rebound_3h <- dt[t > days(4.0) & t < (days(4.0)+hours(3)),]
pl3h <- ethogramPlot(asleep,dt_rebound_3h,treatment,exp,error_bar="sem")#this makes a facet by experiment
print(pl3h)
pl3hmean <- ethogramPlot(asleep,dt_rebound_3h,treatment,error_bar="sem")
print(pl3hmean)

#!!!!!!!!!!!CHECK IF CORRECT!!!!!!!!!! It is ok that key is dt_rebound_3h and not dt?????????????
#this generates a boxplot of the selected datase, could be the entire dt or the subsets. 
dt_sleep_fract_in_rebound = dt_rebound_3h[,.(sleep_fraction=mean(asleep)),by=c("treatment","exp",key(dt_rebound_3h))]
#separating the experiments
pl_boxplot <- ggplot(dt_sleep_fract_in_rebound,aes(treatment, sleep_fraction,fill=treatment)) + geom_boxplot(notch=FALSE) + facet_grid(. ~ exp)
pl_boxplot
#for the mean (I'm not sure how this mean is done...)
pl_boxplotmean <- ggplot(dt_sleep_fract_in_rebound,aes(treatment, sleep_fraction,fill=treatment)) + geom_boxplot(notch=FALSE)
pl_boxplotmean


install.packages("betareg")
library("betareg") 
setwd("C:/Users/ebeckwit/Dropbox/esteban/GG lab/Sleep BEHAVIOR EXPERIMENTS/MERGER of EXPERIMENTS/2015-07-05 + 2015-06-21 + 2015-06-20 - Saori - recovery in non-female tube")

sink(file="stats.txt") #check your working directory because that's where this will go!
#!!!!!!!!!!!CHECK IF CORRECT!!!!!!!!!!
#Statitistics!!!with a linear model. - THE ASSUMPTIONS ARE NOT TESTED!!!
model <- lm(sleep_fraction ~ treatment, dt_sleep_fract_in_rebound)
print(summary(model))# show results
#write(capture.output(summary(model)), "lm.txt") # generets a .txt with the output of the statistical anlisis

#statistics with an anova (again, not sure how wrong this is). - THE ASSUMPTIONS ARE NOT TESTED!!!
ANOVA <- aov(sleep_fraction ~ treatment, data=dt_sleep_fract_in_rebound) 
summary(ANOVA) # show results
#write(capture.output(summary(ANOVA)), "ANOVA.txt") # generets a .txt with the output of the statistical anlisis

tukey<-TukeyHSD(ANOVA)
tukey
#write.csv(data.frame(tukey$`treatment:exp`), 'tukeyresults.txt') # generates a .txt with the tukey output. After the $ put your variable, could be the variables or the interaction
mod <- betareg(sleep_fraction ~ treatment, data=dt_sleep_fract_in_rebound[sleep_fraction!=1 & sleep_fraction!=0])
summary(mod)

#Non parametric
wil <- print(pairwise.wilcox.test(dt_sleep_fract_in_rebound$sleep_fraction, dt_sleep_fract_in_rebound$treatment,p.adjust.method ="bonferroni"))

sink()
