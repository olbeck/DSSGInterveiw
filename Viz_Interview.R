library(ggplot2)
library(dplyr)


########################################
##### Formatting Data
########################################

#Get Data 
#ANDATA = read.table("C:\\Users\\Olivia Beck\\Dropbox\\Olivia\\Conflict\\school\\Honr 499\\UpdatedData\\AllNOS.txt",
#                    header = TRUE)
ANDATA <- read.delim("~/Dropbox/Olivia/Conflict/school/Honr 499/UpdatedData/AllNOS.txt")

#Filter data we care about 
AN <- ANDATA[ANDATA$Week<9,]
AN <- AN[complete.cases(AN),]

#Format Correctly
AN$Date  <-  as.character(AN$Date)
AN$FiscalYear  <-  as.factor(AN$FiscalYear)
AN$WaitTime  <-  as.numeric(AN$WaitTime)
AN$Num  <-  1:nrow(AN)

##################################################
###### Plot Patrons 
##################################################

ggplot(data=AN, mapping = aes(Num, Patrons)) +
  geom_point(aes(color = FiscalYear)) +
  facet_wrap(. ~ Day)+
  xlab("Time")+
  ggtitle("General Timeline of Patrons by Day")

ggplot(data= AN, mapping = aes(Week, Patrons)) +
  geom_point(aes(color = Day)) +
  facet_wrap(. ~ FiscalYear)+
  xlab("Week of Semester")+
  ggtitle("Patrons vs. Week of the Year by Fiscal Year")

ggplot(data=AN, mapping = aes(Week, Patrons)) +
  geom_point(aes(color = FiscalYear)) +
  facet_wrap(. ~ Day)+
  xlab("Week of Semester")+
  ggtitle("Patrons vs. Week of the Year by Day")


##################################################
###### Rides
##################################################

ggplot(data=AN, mapping = aes(Num, Rides)) +
  geom_point(aes(color = FiscalYear)) +
  facet_wrap(. ~ Day) +
  xlab("Time")+
  ggtitle("General Timeline of Rides by Day")

ggplot(data= AN, mapping = aes(Week, Rides)) +
  geom_point(aes(color = Day)) +
  facet_wrap(. ~ FiscalYear)+
  xlab("Week of Semester")+
  ggtitle("Rides vs. Week of the Year by Fiscal Year")

ggplot(data=AN, mapping = aes(Week, Rides)) +
  geom_point(aes(color = FiscalYear)) +
  facet_wrap(. ~ Day)+
  xlab("Week of Semester")+
  ggtitle("Rides vs. Week of the Year by Day")


##################################################
###### Rides
##################################################

ggplot(data=AN, mapping = aes(Num, WaitTime)) +
  geom_point(aes(color = FiscalYear)) +
  facet_wrap(. ~ Day)+
  xlab("Time")+
  ggtitle("Average Wait Time")


ggplot(data= AN, mapping = aes(Cars, WaitTime)) +
  geom_jitter(aes(color = FiscalYear)) +
  facet_wrap(.~ Day, scales = "free")+
  ggtitle("Cars vs. Wait Time")


##########################################
##### By hour Data 
##########################################

#Get Data 
#BY = read.table("C:\\Users\\Olivia Beck\\Dropbox\\Olivia\\Conflict\\school\\Honr 499\\UpdatedData\\ByHour.txt",header = TRUE)
BY <-  read.table("~/Dropbox/Olivia/Conflict/school/Honr 499/UpdatedData/ByHour.txt", header=TRUE)

#Format Data 
BY$Day=as.factor(BY$Day)
BY$Year=as.factor(BY$Year)

#Define functions to get average number of rides 
get_rides <- function(day, year, hour){
  ## INPUT
  # Day = "T", "F", or "S" for Thursday, Friday, or Saturday, respectively 
  # Year = "FY##" for Fiscal year ## (options 17- 19)
  # hour = "Rides##" for hour of night 
  #       -options for Thursday: "Rides10", "Rides11",  "Rides12", "Rides1" 
  #       -options for Friday/Saturday: "Rides10", "Rides11",  "Rides12", "Rides1", "Rides2"
  
  ret <- mean(as.matrix(BY[ BY$Day ==day & BY$Year == year, hour ]))
  return(ret)
}

#Define functions to get average wait time 
get_wait <- function(day, year, wait){
  ## INPUT
  # Day = "T", "F", or "S" for Thursday, Friday, or Saturday, respectively 
  # Year = "FY##" for Fiscal year ## (options 17- 19)
  # hour = "Rides##" for hour of night 
  #       -options for Thursday: "Wait10", "Wait11",  "Wait12", "Wait1" 
  #       -options for Friday/Saturday: "Wait10", "Wait11",  "Wait12", "Wait1", "Wait2"
  
  ret <- mean(as.matrix(BY[ BY$Day ==day & BY$Year == year, wait ]))
  return(ret)
}


#Days, Years, and Hours we care about 
days <- c("T", "F", "S")
years <- c("FY17", "FY18", "FY19")
rides <- c("Rides10", "Rides11",  "Rides12", "Rides1", "Rides2")
waits <- c("Wait10", "Wait11",  "Wait12", "Wait1", "Wait2")

#Create Data Frame 
ARides <- data.frame(day = rep(days, each = 3*5),           #Day of Week
                     year = rep(rep(years, each = 5), 3),   #Fiscal Year
                     ride_hour = rep(rides, 3*3),           #Which Hour for Rides
                     wait_hour = rep(waits, 3*3),           #Which Hour for Wait Time
                     time = as.factor(rep(c(10,11,12,1,2), 3*3)),      #Time of Night
                     rides = rep(NA, 3*3*5),                #Average Rides
                     wait = rep(NA, 3*3*5))                 #Average Wait Time


#remove Thursdays at 2am 
ARides <- ARides %>%
            subset( !(day == "T" & ride_hour == "Rides2"))

#Get Average Rides and Average Wait Time 
for(i in 1:dim(ARides)[1]){
  #Get Current Values 
  d <- ARides$day[i]
  y <- ARides$year[i]
  rh <- ARides$ride_hour[i]
  wh <- ARides$wait_hour[i]
  
  #Get average rides and wait time 
  ARides$rides[i] <- get_rides(d, y , rh)
  ARides$wait[i] <- get_wait(d, y, wh)
}

#Check that it worked 
ARides

#Format for plotting 
ARides$year <- as.factor(ARides$year)
ARides$day <- as.factor(ARides$day)

#Make Plots
ggplot(ARides, mapping = aes(time, wait) ) + 
  geom_point(aes(color = year)) +
  facet_wrap(.~day) +
  ggtitle("Average Wait Over Time") +
  ylab("Average Wait Time") +
  xlab("Time of Night")

ggplot(ARides, mapping = aes(time, rides) ) + 
  geom_point(aes(color = year)) +
  facet_wrap(.~day) +
  ggtitle("Average Rides Completed Over Time") +
  ylab("Average Rides Completed") +
  xlab("Time of Night")


##########################################
##### App Data
##########################################

#Get Data
AN$Percent <- AN$App/ AN$RequestedRides

#Fortmat Data
AppData <- data.frame( Percent = AN$Percent, 
                      FY = AN$FiscalYear, 
                      Day = AN$Day,
                      Num = AN$Num)

AppData <- AppData[complete.cases(AppData),]

#Make Plot
ggplot(data= AppData, mapping = aes(Num, Percent)) + 
  geom_point(aes(color = FY)) + 
  ggtitle("App Usage Over Time") +
  xlab("Time") +
  ylab("App Usage Percentage")




