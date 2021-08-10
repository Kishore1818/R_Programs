#This is global COVID-19 deaths for each country (22 Jan 2020 - 13 July 2020)
#This one circular size indicates the number of deaths.
#I used here SAS output and I mentioned the name of the program in the comments place

library(tidyverse)
library(maps)
library(mapdata)
library(stringr)
library(viridis)
library(sf)
library(gtools)

# reading Global data except US (global_1day.sas)
cvdata <- read.csv("~/Desktop/SASUniversityEdition/myfolders/COVID-19/USAcovid_results/global_upto13july.csv")
dim(cvdata)
head(cvdata)
summary(cvdata)
usdeths <- sum(cvdata$Deaths)
usdeths

# Here reading the US data and US Deaths (global_usstates_1day.sas)
cvdata1 <- read.csv("~/Desktop/SASUniversityEdition/myfolders/COVID-19/USAcovid_results/global_USA_upto13july2020new.csv")
dim(cvdata1)
head(cvdata1)
summary(cvdata1)
restdeths <- sum(cvdata1$Deaths)
restdeths

# Here merging the two datasets into one file
# newdata <- rbind(cvdata,cvdata1, by="Country_Region", all=TRUE)
newdata <- smartbind(cvdata,cvdata1)
dim(cvdata)
dim(cvdata1)
dim(newdata)

summary(newdata)
head(newdata)
tail(newdata)
cdeaths <- newdata$Deaths
cdeaths

png(file="~/Downloads/R_programs/Results/global_deaths_13july2020.png", width=800, height=600)
world_map <- map_data("world")
plot1 <- ggplot(world_map,aes(x=long,y=lat,group=group))+
  geom_polygon(fill="lightgray",color="white") +
  coord_quickmap()+
  guides(fill=FALSE)

plot1

plot1 +
  geom_point(data=newdata,mapping=aes(Long_,Lat,size=Deaths),shape=1,inherit.aes = FALSE,color="purple",alpha=1.0) +
  scale_size_continuous(range=c(1,10), breaks = c(0,250,500,1500,3000,5000,7500,10000,25000))+
  labs(size='Deaths') +
  ggtitle(label='Global COVID-19 Deaths', 
          subtitle = "22 Jan 2020 - 13 July 2020") +
  theme(axis.text.x = element_text(color="black",size=20)) +
  theme(axis.text.y = element_text(color="black",size=20)) +
  xlab("Longitude (deg)") + ylab("Latitude (deg)")

dev.off()