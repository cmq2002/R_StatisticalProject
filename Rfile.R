
#-----------------------------------------------------------------3
# Read in data from csv file
setwd("C:/Users/User/Desktop/4th Semester/Probability and Statistics/Assignment/Final Work/v2")
intel <- read.csv ("MT2013-CC03-03-Cao Minh Quang-DataSet.csv", header = TRUE, sep = ",")
View(intel)

#------------------------------------------------------------------4
# Data Cleaning
install.packages("tidyverse")
library(tidyr)
cleanIntel <- drop_na(intel)
View(cleanIntel)

# Check after cleaning listing range of each variables, check for type
is.numeric(cleanIntel$name)

is.numeric(cleanIntel$launch_date)

table(cleanIntel$cores)
is.numeric(cleanIntel$cores)

table(cleanIntel$threads)
is.numeric(cleanIntel$threads)

table(cleanIntel$bus_speed)
is.numeric(cleanIntel$bus_speed)

table(cleanIntel$base_frequency)
is.numeric(cleanIntel$base_frequency)

table(cleanIntel$turbo_frequency)
is.numeric(cleanIntel$turbo_frequency)

table(cleanIntel$cache_size)
is.numeric(cleanIntel$cache_size)

table(cleanIntel$max_memory_size)
is.numeric(cleanIntel$max_memory_size)

table(cleanIntel$max_temp)
is.numeric(cleanIntel$max_temp)

#--------------------------------------------------------------5.1
# Cut off unnecessary column: id, name, launch_date
newIntel <- cleanIntel[,c("cores","threads","bus_speed","base_frequency","turbo_frequency"
                      ,"cache_size","max_memory_size","max_temp")]
head(newIntel)

#--------------------------------------------------------------5.2
# Descriptive statistic
install.packages("pastecs")
library(pastecs)
stat.desc(newIntel[, c(1,2,3,4,5,6,7,8)]) %>% round(4)

#-------------------------------------------------------------5.3.1
# Visualization with graphs
install.packages("ggpubr")
library("ggpubr")

#Histograms of Cores and Threads
gghistogram(cleanIntel, x = "cores", fill = "blue",
            add = "mean", rug = TRUE, add_density = TRUE)
gghistogram(cleanIntel, x = "threads", fill = "red",
            add = "mean", rug = TRUE, add_density = TRUE)

#---------------------------------------------------------------5.3.2
# stripchart of Base Frequency, Turbo Frequency and Bus speed related to The Number of Cores and Threads
#strip chart for cores
ggstripchart(newIntel,x ='turbo_frequency',y='cores',color='cores')
ggstripchart(newIntel,x ='base_frequency',y='cores',color ='cores')
ggstripchart(newIntel,x ='bus_speed',y='cores',color='cores')
#strip chart for threads
ggstripchart(newIntel,x ='turbo_frequency',y='threads',color='threads')
ggstripchart(newIntel,x ='base_frequency',y='threads',color='threads')
ggstripchart(newIntel,x ='bus_speed',y='threads',color='threads')

#--------------------------------------------------------------5.3.3
install.packages("ggplot2")
install.packages("tidyverse")
library("ggplot2")
library("tidyverse")

#Box plot for cache_size
gathered <- newIntel %>%
pivot_longer(c(cache_size), values_to="KB")
ggplot(gathered,aes(, y = KB)) + geom_boxplot() + labs(x="Cache size", y ="KB" ) + geom_boxplot(fill = 'red')

#Box plot for max_memory_size
gathered <- newIntel %>%
pivot_longer(c(max_memory_size), values_to="KB")
ggplot(gathered,aes(, y = KB)) + geom_boxplot() + labs(x="Max memory size", y ="KB" ) + geom_boxplot(fill = 'red')

#Box plot for bus_speed
gathered <- newIntel %>%
pivot_longer(c(bus_speed), values_to="GT_s")
ggplot(gathered,aes(, y = GT_s)) + geom_boxplot() + labs(x="Bus speed", y ="GT/s" ) + geom_boxplot(fill = 'red')


############################################## ANOVA TEST #################################################################

#----------------------------------------------------------------------------------------7.1
####################################### EFFECT ON BASE FREQUENCY ############################
oneway.Cores = aov(base_frequency ~ cores, data = newIntel)
summary(oneway.Cores)

oneway.Threads = aov(base_frequency ~ threads, data = newIntel)
summary(oneway.Threads)

twoway = aov(base_frequency ~ cores + threads, data = newIntel)
summary(twoway)

# Find the best-fit model
install.packages("raster")
install.packages("AICcmodavg")
library(AICcmodavg)

model.set <- list(oneway.Cores, oneway.Threads, twoway)
model.names <- c("oneway.Cores", "oneway.Threads","twoway")

aictab (model.set, modnames = model.names)

plot(twoway)
# residuals histogram overlaid by normal distribution graph
h <- hist(twoway[[ 'residuals']], breaks = 5, density = 40, col ="red", xlab ="Residuals", main ="Residuals histogram overlaid by
normal distribution graph")
xfit<-seq(min(twoway[['residuals']]), max(twoway[['residuals']]) , length = 40)
yfit<-dnorm(xfit, mean = mean (twoway[['residuals']]), sd = sd(twoway[['residuals']]) )
yfit<-yfit * diff (h$mids[1:2]) * length(twoway[['residuals']])
lines(xfit, yfit, col ="black", lwd = 2)

# Check for normality
shapiro.test(twoway[['residuals']])

# Check for homogenity
library(car)
leveneTest(base_frequency ~ interaction(cores,threads), data=newIntel)

############################## When assumptions of ANOVA is failed ####################
kruskal.test(base_frequency ~ interaction(cores, threads), data=newIntel)
library(rcompanion)
epsilonSquared(x=newIntel$base_frequency, g = interaction(newIntel$cores,newIntel$threads))

# Check which variables observation are statistically significant different
# post-hoc test
install.packages("FSA")
library(FSA)
dunnTest (base_frequency ~ interaction(cores,threads), data = newIntel,method="bh")

############################### When assumptions of ANOVA are verified #############################
# post-hoc test
#twoway.Interaction = aov(base_frequency ~ interaction(cores,threads), data = newIntel)
#TukeyHSD(twoway.Interaction)
#twoway.TukeyHSD = TukeyHSD(twoway.Interaction)
#plot(twoway.TukeyHSD, las = 1)

############################## VISUALIZE THE EFFECT ON BASE FREQUENCY #########################
install.packages("dplyr")
library(dplyr)
meanBase_Freq <- newIntel %>%
  group_by(cores,threads) %>%
  summarise(
    base_frequency = mean(base_frequency)
  )

meanBase_Freq$group <- c("r","a","b","b","b","b","r","b","b","b","c","r","r","r","r","r","r")
meanBase_Freq 
meanBase_Freq = subset(meanBase_Freq, meanBase_Freq$group!="r")
meanBase_Freq

install.packages("ggpubr")
library(ggpubr)
twowayPlot <- ggplot(newIntel,aes(x=cores, y=base_frequency, group=threads))+geom_point(cex=1.5, pch=1.0,position=position_jitter(w=0.1,h=0))
twowayPlot <- twowayPlot+stat_summary(fun.data = 'mean_se',geom='errorbar',width=0.2)+stat_summary(fun.data='mean_se', geom='pointrange')+geom_point(data=meanBase_Freq,aes(x=cores,y=base_frequency))
twowayPlot <- twowayPlot+geom_text(data=meanBase_Freq, label=meanBase_Freq$group, vjust = -8, size = 5) + facet_wrap(~ threads)
twowayPlot <- twowayPlot+theme_classic2()+labs(title = "Base frequency response to the number of cores and threads", x="The number of cores", y="Base Frequency")
twowayPlot

#------------------------------------------------------------------------------------------7.2
####################################### EFFECT ON TURBO FREQUENCY ############################
oneway.Cores = aov(turbo_frequency ~ cores, data = newIntel)
summary(oneway.Cores)

oneway.Threads = aov(turbo_frequency ~ threads, data = newIntel)
summary(oneway.Threads)

twoway = aov(turbo_frequency ~ cores + threads, data = newIntel)
summary(twoway)

# Find the best-fit model
install.packages("raster")
install.packages("AICcmodavg")
library(AICcmodavg)

model.set <- list(oneway.Cores, oneway.Threads)
model.names <- c("oneway.Cores", "oneway.Threads")

aictab (model.set, modnames = model.names)

plot(oneway.Threads)
# residuals histogram overlaid by normal distribution graph
h <- hist(oneway.Threads[[ 'residuals']], breaks = 5, density = 40, col ="red", xlab ="Residuals", main ="Residuals histogram overlaid by normal distribution graph")
xfit<-seq(min(oneway.Threads[['residuals']]), max(oneway.Threads[['residuals']]) , length = 40)
yfit<-dnorm(xfit, mean = mean (oneway.Threads[['residuals']]), sd = sd(oneway.Threads[['residuals']]) )
yfit<-yfit * diff (h$mids[1:2]) * length(oneway.Threads[['residuals']])
lines(xfit, yfit, col ="black", lwd = 2)

# Check for normality
shapiro.test(oneway.Threads[['residuals']])

# Check for homogenity
library(car)
leveneTest(turbo_frequency ~ threads, data=newIntel)

############################## When assumptions of ANOVA is failed ####################
kruskal.test(turbo_frequency ~ threads, data=newIntel)
library(rcompanion)
epsilonSquared(x=newIntel$turbo_frequency, g = interaction(newIntel$threads))

# Check which variables observation are statistically significant different
# post-hoc test
install.packages("FSA")
library(FSA)
dunnTest (turbo_frequency ~ threads, data = newIntel,method="bh")

############################### When assumptions of ANOVA are verified #############################
# post-hoc test
#twoway.Interaction = aov(turbo_frequency ~ interaction(cores,threads), data = newIntel)
#TukeyHSD(twoway.Interaction)
#twoway.TukeyHSD = TukeyHSD(twoway.Interaction)
#plot(twoway.TukeyHSD, las = 1)

#------------------------------------------------------------------------------------------7.3
####################################### EFFECT ON MAX TEMPERATURE ############################
oneway.Cores = aov (max_temp~cores, data = newIntel)
summary(oneway.Cores)
oneway.Threads = aov (max_temp~threads, data = newIntel)
summary(oneway.Threads)

twoway = aov(max_temp~cores + threads, data = newIntel)
summary(twoway)

model.set <- list(oneway.Cores, oneway.Threads, twoway )
model.names <- c("oneway.Cores","oneway.Threads"," twoway")
aictab(model.set, modnames = model.names)
shapiro.test(twoway[['residuals']])
leveneTest(max_temp~interaction(cores,threads),data=newIntel)

kruskal.test(max_temp~interaction(cores,threads),data=newIntel)
library(rcompanion)
epsilonSquared(x=newIntel$max_temp,
               g=interaction(newIntel$cores,newIntel$threads))

# Summarise the original data
meanMax_temp <- newIntel %>%
  group_by(cores,threads) %>%
  summarise(
    base_frequency = mean(max_temp)
  )
# Add the group labels
meanMax_temp$group <- 
  c("r","a","b","b","r","r","r","r","b","b","b","b","b","b","c","r","r")
meanMax_temp 
# Plot the raw data
twowayBSPlot <- ggplot(newIntel,
                       aes(x=cores, y=max_temp, group=threads))
+geom_point(cex=1.5, pch=1.0,
            position=position_jitter(w=0.1,h=0))

# Add means and se to the graph
twowayBSPlot <- twowayBSPlot+
  stat_summary(fun.data = 'mean_se',geom='errorbar',width=0.2)+
  stat_summary(fun.data='mean_se', geom='pointrange')+
  geom_point(data=meanMax_temp,aes(x=cores,y=max_temp))

# Split up the data over the levels of threads
twowayBSPlot <- twowayBSPlot+
  geom_text(data=meanMax_temp, label=meanMax_temp$group, 
            vjust = -8, size = 5)+ 
  facet_wrap(~ threads)

# Generate title
twowayBSPlot <- twowayBSPlot+
  theme_classic2()+
  labs(title = "Max temperature response to the number of cores and threads", 
       x="The number of cores", y="Max temperature")

twowayBSPlot

#------------------------------------------------------------------------------------------7.4
####################################### EFFECT ON BUS SPEED ##################################
onewayBS.Cores = aov(bus_speed ~ cores, data = newIntel)
summary(onewayBS.Cores)

onewayBS.Threads = aov(bus_speed ~ threads, data = newIntel)
summary(onewayBS.Threads)

twowayBS = aov(bus_speed ~ cores + threads, data = newIntel)
summary(twowayBS)

# Find the best-fit model
install.packages("raster")
install.packages("AICcmodavg")
library(AICcmodavg)

modelBS.set <- list(onewayBS.Cores, onewayBS.Threads, twowayBS)
modelBS.names <- c("onewayBS.Cores", "onewayBS.Threads","twowayBS")

aictab (modelBS.set, modnames = modelBS.names)

plot(twowayBS)
# residuals histogram overlaid by normal distribution graph
h <- hist(twowayBS[[ 'residuals']], breaks = 5, density = 40, col ="red", xlab ="Residuals", main ="Residuals histogram overlaid by
normal distribution graph")
xfit<-seq(min(twowayBS[['residuals']]), max(twowayBS[['residuals']]) , length = 40)
yfit<-dnorm(xfit, mean = mean (twowayBS[['residuals']]), sd = sd(twowayBS[['residuals']]) )
yfit<-yfit * diff (h$mids[1:2]) * length(twowayBS[['residuals']])
lines(xfit, yfit, col ="black", lwd = 2)

# Check for normality
shapiro.test(twowayBS[['residuals']])

# Check for homogenity
library(car)
leveneTest(bus_speed ~ interaction(cores,threads), data=newIntel)

############################## When assumptions of ANOVA is failed ####################
kruskal.test(bus_speed ~ interaction(cores, threads), data=newIntel)
library(rcompanion)
epsilonSquared(x=newIntel$bus_speed, g = interaction(newIntel$cores,newIntel$threads))

# Check which variables observation are statistically significant different
# post-hoc test
install.packages("FSA")
library(FSA)
dunnTest (bus_speed ~ interaction(cores,threads), data = newIntel,method="bh")

############################## VISUALIZE THE EFFECT ON BUS SPEED #########################
install.packages("dplyr")
library(dplyr)
meanBus_Speed <- newIntel %>%
  group_by(cores,threads) %>%
  summarise(
    bus_speed = mean(bus_speed)
  )

meanBus_Speed$group <- c("a","b","b","b","b","b","b","b","b","b","b","b","b","b","c","r","r")
meanBus_Speed 
meanBus_Speed = subset(meanBus_Speed, meanBus_Speed$group!="r")
meanBus_Speed

install.packages("ggpubr")
library(ggpubr)
# Plot the raw data
twowayBSPlot <- ggplot(newIntel,aes(x=cores, y=bus_speed, group=threads))+geom_point(cex=1.5, pch=1.0,position=position_jitter(w=0.1,h=0))
# Add means and se to the graph
twowayBSPlot <- twowayBSPlot+stat_summary(fun.data = 'mean_se',geom='errorbar',width=0.2)+stat_summary(fun.data='mean_se', geom='pointrange')+geom_point(data=meanBus_Speed,aes(x=cores,y=bus_speed))
# Split up the data over the levels of threads
twowayBSPlot <- twowayBSPlot+geom_text(data=meanBus_Speed, label=meanBus_Speed$group, vjust = -8, size = 5) + facet_wrap(~ threads)
# Generate title
twowayBSPlot <- twowayBSPlot+theme_classic2()+labs(title = "Bus Speed response to the number of cores and threads", x="The number of cores", y="Bus Speed")
twowayBSPlot



###################################### LINEAR REGRESSION MODEL #############################
#----------------------------------------------------------------------------------------8.2
# Correlation Matrix
test_cor_matrix = newIntel
round(cor(test_cor_matrix),4)

#--------------------------------------------------------------------------------------8.3.1
######################### FITTING CACHE SIZE WITH CORES AND THE OTHERS #####################

# Building Model
library(lmtest)
library(car)
library(lindia)

# Build a model
    Y = newIntel$cache_size
    model = lm(Y ~ cores + bus_speed + base_frequency + turbo_frequency + max_memory_size + max_temp ,data = newIntel)

# view the model
    summary(model)
# plot the model
    plot(model)
# residuals histogram overlaid by normal distribution graph
    h <- hist(model[[ 'residuals']], breaks = 5, density = 40, col ="red", xlab ="Residuals", main ="Residuals histogram overlaid by
normal distribution graph")
    xfit<-seq(min(model[['residuals']]), max(model[['residuals']]) , length = 40)
    yfit<-dnorm(xfit, mean = mean (model[['residuals']]), sd = sd(model[['residuals']]) )
    yfit<-yfit * diff (h$mids[1:2]) * length(model[['residuals']])
    lines(xfit, yfit, col ="black", lwd = 2)

# Multicollinearity test not use when only 1 attribute
    vif(model)  
# Normallity test
    shapiro.test(model[['residuals']])
# Homoscedasticity test
    bptest(model)

#--------------------------------------------------------------------------------------8.3.2
####################### FITTING CACHE SIZE WITH THREADS AND THE OTHERS #####################
Y = newIntel$cache_size
model = lm(Y~threads+bus_speed+base_frequency+turbo_frequency+max_memory_size+max_temp,data=newIntel)
summary(model)
    
plot(model)
h<-hist(model[["residuals"]],breaks=5,density=40,col="red",xlab="Residuals",main="Residuals???histogram???overlaid???by+normal???distribution???graph")
xfit<-seq(min(model[["residuals"]]),max(model[["residuals"]]),length=40)
yfit<-dnorm(xfit,mean=mean(model[["residuals"]]),sd=sd(model[["residuals"]]))
yfit<-yfit*diff(h$mids[1:2])*length(model[["residuals"]])
lines(xfit,yfit,col="black",lwd=2)
    
# Check for Multi - Collinearity
vif(model)

# Check for Normality
shapiro.test(model[["residuals"]])
    
# Check for homogeneity
bptest(model)   

#--------------------------------------------------------------------------------------8.4.1
####################### FITTING MAX MEM SIZE WITH CORES AND THE OTHERS #####################
Y = newIntel$max_memory_size
model = lm(Y~cores+bus_speed+base_frequency+turbo_frequency+cache_size+max_temp,data=newIntel)
summary(model)

plot(model)
h<-hist(model[['residuals']],breaks=5,density=40, col="red", xlab="Residuals" ,main ="Residuals histogram overlaid by normal distribution graph")
xfit<-seq(min(model[['residuals']]),max(model[['residuals']]),length=40)
yfit<-dnorm(xfit, mean=mean(model[['residuals']]) ,sd=sd(model[['residuals']]) )
yfit<-yfit*diff(h$mids[1:2])*length(model[['residuals']])
lines(xfit,yfit,col="black",lwd = 2)

# Check for Multi-Collinearity
vif(model)

# Check for Normality
shapiro.test(model[['residuals']])

# Check for homogeneity
bptest(model)

#--------------------------------------------------------------------------------------8.4.2
##################### FITTING MAX MEM SIZE WITH THREADS AND THE OTHERS #####################
Y = newIntel$max_memory_size
model = lm(Y~threads+bus_speed+base_frequency+turbo_frequency+cache_size+max_temp,data=newIntel)
summary(model)

plot(model)
h<-hist(model[['residuals']],breaks=5,density=40, col="red", xlab="Residuals" ,main ="Residuals histogram overlaid by normal distribution graph")
xfit<-seq(min(model[['residuals']]),max(model[['residuals']]),length=40)
yfit<-dnorm(xfit, mean=mean(model[['residuals']]) ,sd=sd(model[['residuals']]) )
yfit<-yfit*diff(h$mids[1:2])*length(model[['residuals']])
lines(xfit,yfit,col="black",lwd = 2)

# Check for Multi-Collinearity
vif(model)

# Check for Normality
shapiro.test(model[['residuals']])

# Check for homogeneity
bptest(model)    


#--------------------------------------------------------------------------------------8.5.1
####################### FITTING BUS SPEED WITH CORES AND THE OTHERS ########################
# Build a model
YBS = newIntel$bus_speed
modelBS = lm(YBS ~ cores + cache_size + base_frequency + turbo_frequency + max_memory_size + max_temp ,data = newIntel)

# view the model
summary(modelBS)
# plot the model
plot(modelBS)
# Multicollinearity test
vif(modelBS) # not use when only 1 attribute 
# normality test
# residuals histogram overlaid by normal distribution graph
hBS <- hist(modelBS[[ 'residuals']], breaks = 5, density = 40, col ="red", xlab ="Residuals", main ="Residuals histogram overlaid by
normal distribution graph")
xfitBS<-seq(min(modelBS[['residuals']]), max(modelBS[['residuals']]) , length = 40)
yfitBS<-dnorm(xfitBS, mean = mean (modelBS[['residuals']]), sd = sd(modelBS[['residuals']]) )
yfitBS<-yfitBS * diff (hBS$mids[1:2]) * length(modelBS[['residuals']])
lines(xfitBS, yfitBS, col ="black", lwd = 2)
# Sphario - Wik test
shapiro.test(modelBS[['residuals']])
# Homoscedasticity test
# Breusch-Pagan Test
bptest(modelBS)

#--------------------------------------------------------------------------------------8.5.2
##################### FITTING BUS SPEED WITH THREADS AND THE OTHERS ########################
# Build a model
YBS = newIntel$bus_speed
modelBS = lm(YBS ~ threads + cache_size + base_frequency + turbo_frequency + max_memory_size + max_temp ,data = newIntel)

# view the model
summary(modelBS)
# plot the model
plot(modelBS)
# Multicollinearity test
vif(modelBS) # not use when only 1 attribute 
# normality test
# residuals histogram overlaid by normal distribution graph
hBS <- hist(modelBS[[ 'residuals']], breaks = 5, density = 40, col ="red", xlab ="Residuals", main ="Residuals histogram overlaid by
normal distribution graph")
xfitBS<-seq(min(modelBS[['residuals']]), max(modelBS[['residuals']]) , length = 40)
yfitBS<-dnorm(xfitBS, mean = mean (modelBS[['residuals']]), sd = sd(modelBS[['residuals']]) )
yfitBS<-yfitBS * diff (hBS$mids[1:2]) * length(modelBS[['residuals']])
lines(xfitBS, yfitBS, col ="black", lwd = 2)
# Sphario - Wik test
shapiro.test(modelBS[['residuals']])
# Homoscedasticity test
# Breusch-Pagan Test
bptest(modelBS)

#--------------------------------------------------------------------------------------8.6.1
####################### FITTING BASE FREQ WITH CORES AND THE OTHERS ########################
YBF = newIntel$base_frequency
modelBF = lm(YBF~cores+bus_speed+cache-size+turbo_frequency+max_memory_size+max_temp,data=newIntel)
summary(modelBF)
# Check for Normality
shapiro.test(modelBF[["residuals"]])
# Check for homogeneity
bptest(modelBF)

#--------------------------------------------------------------------------------------8.6.2
##################### FITTING BASE FREQ WITH THREADS AND THE OTHERS ########################
YBF = newIntel$base_frequency
modelBF = lm(YBF~threads+bus_speed+cache-size+turbo_frequency+max_memory_size+max_temp,data=newIntel)
summary(modelBF)
# Check for Normality
shapiro.test(modelBF[["residuals"]])
# Check for homogeneity
bptest(modelBF)

#--------------------------------------------------------------------------------------8.7.1
####################### FITTING TURBO FREQ WITH CORES AND THE OTHERS #######################
Y = newIntel$turbo_frequency
model = lm(Y~cores+bus_speed+base_frequency+cache_size+max_memory_size+max_temp,data=newIntel)
summary(model)
plot(model)
h<-hist(model[['residuals']],breaks=5,density=40,col="red", xlab="Residuals",main ="Residuals histogram overlaid by+normal distribution graph")
xfit <-seq(min(model[['residuals']]), max(model[['residuals']]),length=40)
yfit<-dnorm(xfit,mean=mean(model[['residuals']]),sd=sd(model[['residuals']]))
yfit<-yfit*diff(h$mids[1:2])*length(model[['residuals']])
lines(xfit,yfit,col="black",lwd=2)

# Check for Multi-Collinearity
vif(model)

# Check for Normality
shapiro.test(model[['residuals']])

# Check for homogeneity
bptest(model)

#--------------------------------------------------------------------------------------8.7.2
##################### FITTING TURBO FREQ WITH THREADS AND THE OTHERS #######################    
Y = newIntel$turbo_frequency
model = lm(Y~threads+bus_speed+base_frequency+cache_size+max_memory_size+max_temp,data=newIntel)
summary(model)
plot(model)
h<-hist(model[['residuals']],breaks=5,density=40,col="red", xlab="Residuals",main ="Residuals histogram overlaid by+normal distribution graph")
xfit <-seq(min(model[['residuals']]), max(model[['residuals']]),length=40)
yfit<-dnorm(xfit,mean=mean(model[['residuals']]),sd=sd(model[['residuals']]))
yfit<-yfit*diff(h$mids[1:2])*length(model[['residuals']])
lines(xfit,yfit,col="black",lwd=2)

# Check for Multi-Collinearity
vif(model)

# Check for Normality
shapiro.test(model[['residuals']])

# Check for homogeneity
bptest(model)

