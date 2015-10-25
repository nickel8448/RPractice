# In this script I learnt how to get the frequency of data from a CSV. For this, count function was used from the library plyr to put it in a proper data frame
library("plyr")
library("ggplot2")
data <- read.csv("JY.csv", header=TRUE)  # Getting the data from the CSV file
attach(data)
location <- count(Location, "Location")
ggplot(location, aes(x=Location, y=freq, ymax=max(location$freq))) + 
  geom_bar(stat="identity", width=0.8, fill="#B6D7A8") + 
  xlab("States") + 
  ggtitle("States graph for Jagriti Yatra") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.minor = element_blank(), panel.border=element_blank(), panel.background=element_blank()) + 
  geom_text(aes(label=freq), position=position_dodge(width=0.9), vjust=-0.40, size=3) +
  scale_y_continuous(breaks = round(seq(0, max(location$freq), by=20)), "Number of People") +
  ggsave("plot.png", dpi=300)
