#This program for calculate the age group COVID-19 deaths and pyramid plot

#Loading libraries
library(ggplot2)
library(ggthemes)
options(scipen = 999)  # turns of scientific notations like 1e+40

# preparing the csv file using Covid_percentages.sas
cvdata <- read.csv("~/Downloads/R_programs/covid_gender_deaths_age_per_NYC.csv")
dim(cvdata)
head(cvdata)
summary(cvdata)
pdata <-(cvdata$Users)
pdata1<-abs(pdata)
pdata1
pdata1[pdata1 <= 1.2] <- NA

#Saving the image file as a png format
png(file="~/Downloads/R_programs/Results/death_age_gender_pyramid_NYCplot.png", width=800, height=600)
# X Axis Breaks and Labels 
brks <- seq(-20,20, 5)
lbls = paste0(as.character(c(seq(20, 0, -5), seq(5, 20, 5))), "%")
lbls

# Plot
ggplot(cvdata, aes(x = Age, y = Users, fill = Gender)) +   # Fill column
  geom_bar(stat = "identity", width = .5) +   # draw the bars
  scale_y_continuous(breaks = brks,   # Breaks
                     labels = lbls) + # Labels
  coord_flip() +  # Flip axes
  labs(title="USA: New York City COVID-19 Deaths (22 Jan 2020 - 14 July 2020)") +
  theme_tufte() +  # Tufte theme from ggfortify
  theme(plot.title = element_text(hjust = 0.5,size=20), 
        axis.ticks = element_blank(),
        text=element_text(color="black",size=20),
        legend.position = c(0.76,0.2)) +   # Centre plot title
  scale_fill_brewer(palette = "Dark2")+  # Color palette
  ylab("Death rate (%)") + xlab("Age")+
  geom_text(aes(label=pdata1),position=position_dodge(width=0.5),vjust=-0.18,hjust=0.15,angle=270,size=3)

dev.off()

