#This one is a circular map for the COVID-19 project
#Data cleaning purpose use Covid_functions.r
#This map shows Confirmed, Deaths, and Recovered COVID-19 cases with different colors of all USA states.

# First run the circular_varplot.r and then use this
library(ggplot2)
library(formattable)

prgname = "circular_plot_covid.r"
# # reading the external dataset (usstates_deathrates.sas deaths up to 13 July 2020)
cvdata <- read.csv("/home/jayanthikishore/Desktop/SASUniversityEdition/myfolders/COVID-19/USAcovid_results/USA_13july2020_statewise.csv")
dim(cvdata)
head(cvdata)
summary(cvdata)
nn <- nrow(cvdata)
cvdata

group=c( rep('A', 10), rep('B', 20), rep('C', 14), rep('D', 7))
statenames <- cvdata$Province_State
value1 <- log(cvdata$Confirmed)
value2 <- log(cvdata$Deaths)
value3 <- log(cvdata$Recovered)

value3[which(!is.finite(value3))] <- 0

data <- data.frame(individual=statenames, group=group,value1=value1, value2=value2, value3=value3)
dim(data)
data

# Transform data in a tidy format (long format)
data <- data %>% gather(key = "observation", value="value", -c(1,2)) 

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
nObsType <- nlevels(as.factor(data$observation))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group)*nObsType, ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar*nObsType )
data <- rbind(data, to_add)
data <- data %>% arrange(group, individual)
data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data <- data %>% group_by(id, individual) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 180 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(idm), end=max(idm) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p <- ggplot(data) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=observation), stat="identity", alpha=0.5) +
  # scale_fill_viridis(discrete=TRUE) +
  scale_fill_manual(values = c("blue4", "red", "green"),labels=c("Confirmed","Deaths","Recovered")) +
  labs(x = "", fill = "log(COVID-19)") +
  # Add a val=25/20/15/10/5 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 5, xend = start, yend = 5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 15, xend = start, yend = 15), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 25/20/15/10/5 lines
  ggplot2::annotate("text", x = rep(max(data$id),6), y = c(0, 5, 10, 15, 20,25), label = c("0", "5", "10", "15", "20","25") , color="grey", size=6 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-10,max(label_data$tot, na.rm=T)) +
  theme_minimal() +
  
  theme(legend.position=c(0.5,0.5),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())+
  # theme(
  #   legend.position = "none",
  #   axis.text = element_blank(),
  #   axis.title = element_blank(),
  #   panel.grid = element_blank(),
  #   plot.margin = unit(rep(-1,4), "cm") 
  # ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+2, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=4, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -10, xend = end, yend = -10), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )+
  # geom_text(data=base_data, aes(x = title, y = -6, label=group), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=3, fontface="bold", inherit.aes = FALSE)
  
  labs(caption=prgname) +
  theme(plot.caption=element_text(size=10, hjust=0, margin=margin(15,0,0,0)))

# Save at png
ggsave(p, file="~/Downloads/R_programs/Results/circular_barplot_deaths_recover_recover_13jul.png", width=10, height=10)