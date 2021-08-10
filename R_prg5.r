#This one is for confirmed cases for each county in the USA and Plotting
#The bubble size shows the number of confirmed cases

library(tidyverse)
library(maps)
library(mapdata)
library(stringr)
library(viridis)
library(sf)

#reading the USA state meta data 
states <- map_data("state")
dim(states)
head(states) #print(states)
tail(states)

png(file="~/Downloads/R_programs/Results/USA_confirmed_cases_13July 2020.png", width=800, height=600)
# plot all states, all colored with differently
gg1 <- ggplot(data = states)
  geom_poly(aes(x=long,y=lat,fill=region,group=group),color="white")+
  coord_quickmap()
  guides(fill=FALSE)
gg1

# reading the external dataset ((uscounties_1day.sas))
cvdata <- read.csv("/home/jayanthikishore/Desktop/SASUniversityEdition/myfolders/COVID-19/USAcovid_results/uscounties_1day_23june.csv")
dim(cvdata)
head(cvdata)
summary(cvdata)

# adding the covid-19 confirmed cases to ggplot (gg1)
gg1 +
  geom_point(data=cvdata,aes(x=Long_,y=Lat,size=Confirmed)) +
  ggtitle(label='USA each county COVID-19 Confirmed cases', 
          subtitle = "22 Jan 2020 - 13 July 2020") +
  theme(axis.text.x = element_text(color="black",size=12)) +
  theme(axis.text.y = element_text(color="black",size=12)) +
  xlab("Longitude (deg)") + ylab("Latitude (deg)")
dev.off()