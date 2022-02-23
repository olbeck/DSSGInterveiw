library(tidyverse)
library(ggmap)

#############################
#### Set Up data FY2019 
##############################

#Import Data
#RideLogFY19 <- read.csv("C:/Users/Olivia Beck/Dropbox/Olivia/Conflict/school/Honr 499/Maps/RideLogFY19_8Weeks.csv", header = TRUE)
RideLogFY19 <- read.csv("~/Dropbox/Olivia/Conflict/school/Honr 499/Maps/RideLogFY19_8Weeks.csv")

#Formatting Pick up Data
PickUp <- data.frame(both = RideLogFY19$Pickup..lat.lon.)
PickUp %>%
  separate(both, c("Lat", "Long"), sep = ",") -> PickUp_Sep
PickUp_Sep$Lat <- as.numeric(PickUp_Sep$Lat)
PickUp_Sep$Long <- as.numeric(PickUp_Sep$Long)
dim(PickUp_Sep)

#Number of rides picked up at Old Town
sum(RideLogFY19$Pick.Up.Location == "Old Town (Mountain and Remington)")

#Formatting Drop of Data
DropOff <- data.frame(both = RideLogFY19$Dropoff..lat.lon.)
DropOff %>%
  separate(both, c("Lat", "Long"), sep = ",") -> DropOff_Sep
DropOff_Sep$Lat <- as.numeric(DropOff_Sep$Lat)
DropOff_Sep$Long <- as.numeric(DropOff_Sep$Long)
dim(DropOff_Sep)

#Number of rides dropped off at Old Town
sum(RideLogFY19$Drop.Off.Location == "Old Town (Mountain and Remington)")


#############################
###### Set up data FY20
#############################

#Get Data
#RideLogFY20 <- read.csv("C:/Users/Olivia Beck/Dropbox/Olivia/Conflict/school/Honr 499/Maps/RawData/FY20.csv", header = TRUE)
RideLogFY20 <- read.csv("~/Dropbox/Olivia/Conflict/school/Honr 499/Maps/RawData/FY20.csv")

#Format Pick up data
PickUp20 <- data.frame(both = RideLogFY20$Pickup..lat.lon.)
PickUp20 %>%
  separate(both, c("Lat", "Long"), sep = ",") -> PickUp_Sep20
PickUp_Sep20$Lat <- as.numeric(PickUp_Sep20$Lat)
PickUp_Sep20$Long <- as.numeric(PickUp_Sep20$Long)
dim(PickUp_Sep20)

#Number of Pick ups at Old Town 
sum(RideLogFY20$Pick.Up.Location == "Old Town (Mountain and Remington)")

#Format Drop off Data
DropOff20 <- data.frame(both = RideLogFY20$Dropoff..lat.lon.)
DropOff20 %>%
  separate(both, c("Lat", "Long"), sep = ",") -> DropOff_Sep20
DropOff_Sep20$Lat <- as.numeric(DropOff_Sep20$Lat)
DropOff_Sep20$Long <- as.numeric(DropOff_Sep20$Long)
dim(DropOff_Sep20)

#number of drop off's at Old Town 
sum(RideLogFY20$Drop.Off.Location == "Old Town (Mountain and Remington)")


########################################################################################################################################
#For mapping: 
ggmap::register_google(key = "AIzaSyC5RjNsKxsVNNHZOSzAIZuVbTygd3wLSJo")




#######################################################
########Pick Up Maps FY19
#######################################################

## Specify a map with center at the center of all the coordinates
med.longitude <- median(PickUp_Sep$Long)
med.latitude <- median(PickUp_Sep$Lat)
pickup.map <- get_map(location = c(med.longitude, med.latitude), zoom = 14, scale = 2)

#Get the Map
PickUpMap19 <- ggmap(pickup.map) +
            coord_cartesian() +
            geom_hex(data=subset(PickUp_Sep, Long > -105.12 & Long < -105.06 & Lat>40.55 & Lat<40.6) ,aes(x=(Long), y=(Lat), fill = log(..count..)), alpha = 0.85) +
            ggtitle("Pick Up Requests From Fall 2018") + 
            labs(fill='Log Frequency (in Hundreds)') +
            xlab("Longitude") +
            ylab("Latitude") +  
            scale_fill_viridis() + theme_bw()
## Adding alpha makes it look bad 
  
PickUpMap19

#######################################################
########Drop Of FY19######### 
#######################################################

#Center the Map
med.longitude <- median(DropOff_Sep$Long)
med.latitude <- median(DropOff_Sep$Lat)
DropOff.map <- get_map(location = c(med.longitude, med.latitude), zoom = 14, scale = 2)

## Get the Map
DropOffMap19 <- ggmap(pickup.map) +
  coord_cartesian() +
  geom_hex(data=subset(DropOff_Sep, Long > -105.12 & Long < -105.06 & Lat>40.55 & Lat<40.6) ,aes(x=(Long), y=(Lat), fill = log(..count..)), alpha = 0.85) +
  ggtitle("Drop Off Request From Fall 2019") + 
  labs(fill='Log Frequency (in Hundreds)') +
  xlab("Longitude") +
  ylab("Latitude")+  
  scale_fill_viridis() + theme_bw()
## Adding alpha makes it look bad 

DropOffMap19

#######################################################
##### Pick up FY20#######
#######################################################

## Specify a map with center at the center of all the coordinates
med.longitude <- median(PickUp_Sep20$Long)
med.latitude <- median(PickUp_Sep20$Lat)
pickup.map20 <- get_map(location = c(med.longitude, med.latitude), zoom = 14, scale = 2)

#Get the Map
PickUpMap20 <- ggmap(pickup.map) +
  coord_cartesian() +
  geom_hex(data=subset(PickUp_Sep20, Long > -105.12 & Long < -105.06 & Lat>40.55 & Lat<40.6) ,aes(x=(Long), y=(Lat), fill = log(..count..)), alpha = 0.85) +
  ggtitle("Pick Up Requests From Fall 2019") + 
  labs(fill='Log Frequency (in Hundreds)') +
  xlab("Longitude") +
  ylab("Latitude")+  
  scale_fill_viridis() + theme_bw()
## Adding alpha makes it look bad 

PickUpMap20

#######################################################
####### Drop Off FY20##### 
#######################################################

#Center the Map
med.longitude <- median(DropOff_Sep20$Long)
med.latitude <- median(DropOff_Sep20$Lat)
DropOff.map20 <- get_map(location = c(med.longitude, med.latitude), zoom = 14, scale = 2)

#Get the Map 
DropOffMap20 <- ggmap(pickup.map) +
  coord_cartesian() +
  geom_hex(data=subset(DropOff_Sep20, Long > -105.12 & Long < -105.06 & Lat>40.55 & Lat<40.6) ,aes(x=(Long), y=(Lat), fill = log(..count..)), alpha = 0.85) +
  ggtitle("Drop off Requests From Fall 2019") + 
  labs(fill='Log Frequency (in Hundreds)') +
  xlab("Longitude") +
  ylab("Latitude")+  
  scale_fill_viridis() + theme_bw()
## Adding alpha makes it look bad 

DropOffMap20

