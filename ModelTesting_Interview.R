#### Load In Data ########
#ANDATA = read.table("C:\\Users\\Olivia Beck\\Dropbox\\Olivia\\Conflict\\school\\Honr 499\\UpdatedData\\AllNOS.txt",
#                    header = TRUE
#)
ANDATA <- read.delim("~/Dropbox/Olivia/Conflict/school/Honr 499/UpdatedData/AllNOS.txt")

AN8=ANDATA[ANDATA$Week<9,]
AN8=AN8[complete.cases(AN8),]
AN8$Week = as.factor(AN8$Week)
AN8$WaitTime = as.numeric(AN8$WaitTime)

####Load Libraries######
library(devtools)
install_github("olbeck/saferds")
library(saferds)
library(plotrix)
library(plyr)

#############################################
##### Model 1: Indicator
##############################################

#Define Variables
set.seed(700)
XMat = model.matrix(~ AN8$Week + AN8$Day + I(AN8$FiscalYear=="FY20") + AN8$Week*AN8$Day)
Y = as.matrix(AN8$Rides)

#Define Global Parameters 
tau2  <- 2#2^2
mu=matrix(rep(0,ncol(XMat)))
logmu0 <-log(mean(Y))
S=50000

#run the model 
Beta_Sim <- post_est(XMat, Y, mu, tau2, logmu0, S, depend=TRUE)

#get estimate and CI
Beta_Sim_1_mean <- mean(exp(Beta_Sim[[3]][,11]))
Beta_Sim_1_CI <- quantile(exp(Beta_Sim[[3]][,11]),c(0.025,0.975))

#find which variable in the data matrix we are about 
colnames(XMat)  ### Col 11 is the one we care about 

## Just the ones we care about
plotCI(11, Beta_Sim_1_mean, Beta_Sim_1_mean-Beta_Sim_1_CI[1], -Beta_Sim_1_mean+Beta_Sim_1_CI[2])

Beta_Sim_1_mean
Beta_Sim_1_CI

#Diagnostics
plot(Beta_Sim[[3]][,11], type = "l", ylab = "Fall 2019 Indicator", main = "Model 1")




#######################################################
##### Model 2: Indicator and Years 
#######################################################

#Get data we care about
AN8Copy <- AN8

AN8Copy$FiscalYear <- mapvalues(AN8Copy$FiscalYear, from = c("FY14", "FY15", "FY16", "FY17", "FY18", "FY19", "FY20"), to = c(1:7) )
AN8Copy$FiscalYear <- as.numeric(AN8Copy$FiscalYear)

#Define Variables
XMat = model.matrix(~ AN8Copy$Week + AN8Copy$Day + AN8Copy$FiscalYear + I(AN8$FiscalYear=="FY20") + AN8Copy$Week*AN8Copy$Day)
Y = as.matrix(AN8$Rides)

#Define Global Parameters 
tau2  <- 2#2^2
mu=matrix(rep(0,ncol(XMat)))
logmu0 <-log(mean(Y))
S=50000

#Run the Model
Beta_Sim <- post_est(XMat, Y, mu, tau2, logmu0, S, depend=TRUE)

#find which variable in the data matrix we are about 
colnames(XMat)  ### Col 11, 12 is the one we care about 

#Get Estimates and CI's 
Beta_Sim_2_mean <- mean(exp(Beta_Sim[[3]][,12]))# 12 is the Indicator 
Beta_Sim_2_CI <- quantile(exp(Beta_Sim[[3]][,12]),c(0.025,0.975)) 

Beta_Sim_22_mean <- mean(exp(Beta_Sim[[3]][,11])) # 11 is the year 
Beta_Sim_22_CI <- quantile(exp(Beta_Sim[[3]][,11]),c(0.025,0.975))

## Look at jus the indicator we care about
plotCI(12, Beta_Sim_2_mean, Beta_Sim_2_mean-Beta_Sim_2_CI[1], -Beta_Sim_2_mean+Beta_Sim_2_CI[2])
Beta_Sim_2_mean 
Beta_Sim_2_CI 

##Diagnostics
plot(Beta_Sim[[3]][,12], type = "l", ylab = "Fall 2019 Indicator", main = "Model 2")

## Look at Other Variables
plotCI(12, Beta_Sim_22_mean, Beta_Sim_22_mean-Beta_Sim_22_CI[1], -Beta_Sim_22_mean+Beta_Sim_22_CI[2])

Beta_Sim_22_mean 
Beta_Sim_22_CI
plot(Beta_Sim[[3]][,11], type = "l") #Diagnostics

###########################################################
##### Model 3: FY19- FY20 ONLY, Indicator
###########################################################

#Get Data 
Dat_shrink <- AN8Copy[AN8Copy$FiscalYear>5, ]

#Define Variables
XMat = model.matrix(~ Dat_shrink$Week + Dat_shrink$Day + Dat_shrink$Week*Dat_shrink$Day + I(Dat_shrink$FiscalYear=="7") )
XMat <- XMat[,-c(which(colSums(XMat) == 1))]
Y = as.matrix(Dat_shrink$Rides)

#Define Global Parameters
tau2  <- 2#2^2
mu=matrix(rep(0,ncol(XMat)))
logmu0 <-log(mean(Y))
S=50000

#find the col for the variable we care about 
colnames(XMat)  ### Col 11 is the one we care about 

Beta_Sim <- post_est(XMat, Y, mu, tau2, logmu0, S, depend=TRUE)
Beta_Sim_3_mean <- mean(exp(Beta_Sim[[3]][,11]))
Beta_Sim_3_CI <- quantile(exp(Beta_Sim[[3]][,11]),c(0.025,0.975))


## Just the indicator we care about
plotCI(11, Beta_Sim_3_mean, Beta_Sim_3_mean-Beta_Sim_3_CI[1], -Beta_Sim_3_mean+Beta_Sim_3_CI[2])

Beta_Sim_3_mean
Beta_Sim_3_CI

#Diagnostics
plot(Beta_Sim[[3]][,11], type = "l", ylab = "Fall 2019 Indicator", main = "Model 3")

###########################################################
##### Model 4: Indicator, Home Game  #######
###########################################################

#Get Data
AN8$HomeGame <- AN8$Canvas +AN8$Hughes

#Defnie the variables 
XMat = model.matrix(~ AN8$Week + AN8$Day + I(AN8$FiscalYear=="FY20") + I(AN8$HomeGame == 1) + AN8$Week*AN8$Day)
Y = as.matrix(AN8$Rides)

#Define the Global Parameters
tau2  <- 2#2^2
mu=matrix(rep(0,ncol(XMat)))
logmu0 <-log(mean(Y))
S=50000

#find the columns we care about 
colnames(XMat)  ### Col 11, 12 is the one we care about 

#Run the model
Beta_Sim <- post_est(XMat, Y, mu, tau2, logmu0, S, depend=TRUE)

#Get Estimates and CI's 
Beta_Sim_4_mean <- mean(exp(Beta_Sim[[3]][,11]))
Beta_Sim_4_CI <- quantile(exp(Beta_Sim[[3]][,11]),c(0.025,0.975))

Beta_Sim_4_mean_fb <- mean(exp(Beta_Sim[[3]][,12]))
Beta_Sim_4_CI_fb <- quantile(exp(Beta_Sim[[3]][,12]),c(0.025,0.975))


## Just the indicators we care about
plotCI(11, Beta_Sim_4_mean, Beta_Sim_4_mean-Beta_Sim_1_CI[1], -Beta_Sim_4_mean+Beta_Sim_4_CI[2])
Beta_Sim_4_mean
Beta_Sim_4_CI

#Diagnostics: Trace Plot
plot(Beta_Sim[[3]][,11], type = "l", ylab = "Fall 2019 Indicator", main = "Model 4")

#Other Variables 
plotCI(12, Beta_Sim_4_mean_fb, Beta_Sim_4_mean_fb-Beta_Sim_4_CI_fb[1], -Beta_Sim_4_mean_fb+Beta_Sim_4_CI_fb[2])
Beta_Sim_4_mean_fb
Beta_Sim_4_CI_fb #home football doesn't change rides


########################################################
##### Model 5: Indicator, Canvas VS Huges
#########################################################

#Get Data
AN8$HomeGame <- AN8$Canvas +AN8$Hughes

#Define Variables
XMat = model.matrix(~ AN8$Week + AN8$Day + I(AN8$FiscalYear=="FY20") + I(AN8$Canvas == 1) + I(AN8$Hughes) + I(AN8$Showdown) + AN8$Week*AN8$Day)
Y = as.matrix(AN8$Rides)

#Define Global Parameters
tau2  <- 2#2^2
mu=matrix(rep(0,ncol(XMat)))
logmu0 <-log(mean(Y))
S=50000

#Run Model
Beta_Sim <- post_est(XMat, Y, mu, tau2, logmu0, S, depend=TRUE)

#Get Estimates and CI's 
Beta_Sim_5_mean <- mean(exp(Beta_Sim[[3]][,11]))
Beta_Sim_5_CI <- quantile(exp(Beta_Sim[[3]][,11]),c(0.025,0.975))

Beta_Sim_5_mean_canvas <- mean(exp(Beta_Sim[[3]][,12]))
Beta_Sim_5_CI_canvas <- quantile(exp(Beta_Sim[[3]][,12]),c(0.025,0.975))

Beta_Sim_5_mean_huges <- mean(exp(Beta_Sim[[3]][,13]))
Beta_Sim_5_CI_huges <- quantile(exp(Beta_Sim[[3]][,13]),c(0.025,0.975))

Beta_Sim_5_mean_sd <- mean(exp(Beta_Sim[[3]][,14]))
Beta_Sim_5_CI_sd <- quantile(exp(Beta_Sim[[3]][,14]),c(0.025,0.975))

#Find the indicators we care about
colnames(XMat)  ### Col 11, 12 is the one we care about 

## Just the indicator we care about
plotCI(11, Beta_Sim_4_mean, Beta_Sim_4_mean-Beta_Sim_1_CI[1], -Beta_Sim_4_mean+Beta_Sim_4_CI[2])
Beta_Sim_4_mean
Beta_Sim_4_CI

#Trace Plot
plot(Beta_Sim[[3]][,11], type = "l" , ylab = "Fall 2019 Indicator", main = "Model 5")


#Other Variables 
plotCI(12, Beta_Sim_5_mean_canvas, Beta_Sim_5_mean_canvas-Beta_Sim_5_CI_canvas[1], -Beta_Sim_5_mean_canvas+Beta_Sim_5_CI_canvas[2])
Beta_Sim_5_mean_canvas
Beta_Sim_5_CI_canvas #Canvas Games decreases rides 


plotCI(12, Beta_Sim_5_mean_huges, Beta_Sim_5_mean_huges-Beta_Sim_5_CI_huges[1], -Beta_Sim_5_mean_huges+Beta_Sim_5_CI_huges[2])
Beta_Sim_5_mean_huges
Beta_Sim_5_CI_huges #huges increases rides

plotCI(12, Beta_Sim_5_mean_sd, Beta_Sim_5_mean_sd-Beta_Sim_5_CI_sd[1], -Beta_Sim_5_mean_sd+Beta_Sim_5_CI_sd[2])
Beta_Sim_5_mean_sd
Beta_Sim_5_CI_sd #show down games decrease rides like crazy 


################################################### 
#######EPORT
#################################################### 
report <- matrix(rep(NA, 15), nrow = 5, ncol = 3)
report[1, ] <- c(Beta_Sim_1_mean, Beta_Sim_1_CI)
report[2, ] <- c(Beta_Sim_2_mean, Beta_Sim_2_CI)
report[3, ] <- c(Beta_Sim_3_mean, Beta_Sim_3_CI)
report[4, ] <- c(Beta_Sim_4_mean, Beta_Sim_4_CI)
report[5, ] <- c(Beta_Sim_5_mean, Beta_Sim_5_CI)
rownames(report) <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5")
colnames(report) <- c("Mean", "Lower CI", "Upper CI")
report

