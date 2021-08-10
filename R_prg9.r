#This one is for histgrams plot with different age groups and different races
#For plotting I used SAS program output 

cvdata <- read.csv("/home/jayanthikishore/Desktop/SASUniversityEdition/myfolders/COVID-19/USAcovid_results/USA_race_age_deaths_wpop_24June20.csv")
dim(cvdata)
head(cvdata)
summary(cvdata)
totpop <- cvdata$Male_pop + cvdata$Female_pop
totpop

groups <- cvdata$Race_and_Hispanic_Origin_Group
bars <- cvdata$Age_group
# Death per 100000 people
values <- 100000*(cvdata$COVID_19_Deaths/totpop)
values

dd <- data.frame(grps=groups, bars=bars,values=values)
dim(dd)
dd

png(file="~/Downloads/R_programs/Results/USA_age_race_death_rate.png", width=800, height=600)
library(ggplot2)
ggplot(dd, aes(x=bars, y=values, fill=bars)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~grps, ncol=4) +
  ggtitle(label='USA Race and age COVID-19 Death rate per 100000 people', 
          subtitle = "22 Jan 2020 - 24 June 2020") +
  scale_y_continuous(breaks = round(seq(0, max(values), by = 200),1))+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  theme(axis.text.y = element_text(color="black",size=16))+
  theme(axis.text.x = element_text(color="black",size=13))+
  theme(strip.text = element_text(size = 16))+
  theme(axis.title = element_text(size = 20)) +
  xlab(" ") + ylab("Death rate") +
  theme(legend.title = element_text(color = "blue", size = 14),
        legend.text = element_text(color = "black", size = 14)) +
  theme(legend.title=element_blank())
dev.off()
