#This program for confirmed and deaths percentages over USA

library(ggplot2)
library(formattable)

prgname = "usa_states_percentages.r"
# # reading the external dataset (covid=19 confirmed and deaths up to 12 June 2020)
cvdata <- read.csv("/home/jayanthikishore/Desktop/SASUniversityEdition/myfolders/COVID-19/USAcovid_results/USA_13july2020_statewise_per.csv")
dim(cvdata)
head(cvdata)
summary(cvdata)
nn <- nrow(cvdata)
nn

# state plot background and COVID-19 pie chart
# # https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
states <- map_data("state")
# dim(states)
# head(states) #print(states)
# tail(states)

png(file="~/Downloads/R_programs/Results/USA_confirmed_deathspercentage_13july2020_piemap1.png", width=800, height=600)
us <- map_data('state')
gg1 <- ggplot(us, aes(long, lat)) +
  geom_map(map=us, fill="grey97", color="grey") +
  coord_quickmap()+
  guides(fill=FALSE) 

deathper <- (formattable(cvdata$Death_per, digits = 1, format = "f"))
ded <- (formattable(percent(deathper/100.),digits=2,format="f"))
recvrper <- (formattable(cvdata$Recover_per, digits = 1, format = "f"))
gg1 + geom_point(aes(x=Long_,y=Lat+0.6),data=cvdata,size=1) + 
  geom_text(data=cvdata,aes(x=Long_,y=Lat,label=paste(deathper)),color="red",size=3,parse=TRUE) +
  geom_text(data=cvdata,aes(x=Long_,y=Lat-0.6,label=paste(recvrper)),color="blue",size=3,parse=TRUE) +
  annotate("text", label="Death percentage",x=-112.,y=28,size=5,color="red") +
  annotate("text", label="Recover percentage",x=-112.,y=27,size=5,color="blue") +
  annotate("text", label=prgname,x=-112.,y=23,size=3,color="black") + 
  labs(title = "Each state COVID-19 cases: Death percentage",
       subtitle = "22 Jan 2020 - 13 July 2020",
       caption = "Data source: WHO_ICTRP",
       FILL = NULL) + labs(size='Deaths') +
  theme_bw() +
  theme(legend.position = c(0.9,0.08),
        legend.justification = c(1,0),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
dev.off()