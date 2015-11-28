library("ggplot2")
library("reshape2")
library("stringr")  # Library to strip the string
library("plyr")
library("dplyr")

ns <- read.csv("NS.csv")  # Reading the CSV
# Computing Days with Number of News Pages

themefunc <- function()
{
  ##Function for the theming of all the graphs
  themev <- theme(panel.background = element_blank(),
                  axis.text.x = element_text(size = 11.5, colour="black"),
                  axis.text.y = element_text(size = 11.5, colour="black"),
                  axis.title.y = element_text(size = 14.5, colour="black", vjust = 1.5),
                  axis.title.x = element_text(size = 14.5, colour="black", vjust=-.5)) # No background
  return(themev)
}

#####Pages Composition####
# Make a new data frame
newsPages <- data.frame(ns$Date, ns$Full.Page.Ads, I(ns$Total.Pages - ns$Full.Page.Ads))
# Rename the Columns
names(newsPages) <- c("Date", "FullPageAds", "NewsPages")
# Melt the Date
newsPages2 <- melt(newsPages)
# Reorder it based on the date
newsPages2 <- transform(newsPages2, Date = reorder(Date, order(as.Date(newsPages2$Date, format="%d/%m/%Y"), decreasing = TRUE)))
# Making a new variable label_y to know the point where label should be
newsPages2 <- ddply(newsPages2, "Date", mutate, label_y = cumsum(value) - .5*value)
# Making the plot
ggplot(data = newsPages2, aes(x = Date, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  ylab("Total Number of Pages") +  # putting the label for y axis
  scale_fill_manual(values = c("#424242", "#2E7D32"),  # Chaning the colour of the bars
                    name = "Pages Composition",  # Name of the Graph
                    breaks = c("FullPageAds", "NewsPages"),  # Name of the legend
                    labels = c("Full Page Ads", "Pages with News")) +  # Changing the legend
  scale_y_continuous(breaks = round(seq(0, max(30), by=3))) +
  geom_text(aes(y=label_y, label=value), vjust=0.5, color='white', fontface=1, size=3) +  # Putting the labels
  themefunc() +
  ggtitle("Composition of Pages in the Newspaper") +
  theme(plot.title = element_text(lineheight=.8, vjust = 1.5)) +
  coord_flip() +  # Flipping the graph
  ggsave("Ads.png", dpi=400)

#####

# Weeks and Numbers of ads for every week day Variable Programming

AllDayAds <- data.frame(ns$Full.Page.Ads, ns$Day, ns$Total.Pages)
days <- vector(mode = "character", length=0)
daysFreq <- vector(mode = "integer", length=0)
totalPages <- vector(mode = "integer", length=0)
for (i in 1:nrow(AllDayAds))
{
  if(as.character(AllDayAds[i, 2]) %in% as.character(days))  # Checking the day is already present in the vector days
  {
    daysFreq[match(as.character(AllDayAds[i, 2]), days)] = I(daysFreq[match(as.character(AllDayAds[i, 2]), days)] + AllDayAds[i, 1])
    totalPages[match(as.character(AllDayAds[i, 2]), days)] = I(totalPages[match(as.character(AllDayAds[i, 2]), days)] + AllDayAds[i, 3])
  }
  else
  {
    days <- c(days, as.character(AllDayAds[i, 2]))
    daysFreq <- c(daysFreq, AllDayAds[i, 1])
    totalPages <- c(totalPages, AllDayAds[i, 3])
  }
}

# Making a new data frame for the Days and Number of ads
AllDayAds <- data.frame(days, daysFreq, totalPages)
names(AllDayAds) <- c("WDay", "Pages", "TotalPages")
# Increasing the number of pages for Friday as there was a holiday and averaging
for (i in 1:nrow(AllDayAds))
{
  if (as.character(AllDayAds[i, 1]) == "Friday")
  {
    AllDayAds[i, 2] = round(AllDayAds[i, 2] + (AllDayAds[i, 2]/3), 0)
    AllDayAds[i, 3] = round(AllDayAds[i, 3] + (AllDayAds[i, 3]/3), 0)
  }
  AllDayAds[i, 2] = round(AllDayAds[i, 2] / 4, 0)
  AllDayAds[i, 3] = round(AllDayAds[i, 3] / 4, 0)
}

#####

# Days of Week and Numbers of ads for every week day Plotting

meltedAllDayAds <- melt(AllDayAds)
meltedAllDayAds <- ddply(meltedAllDayAds, "WDay", mutate, label_y = cumsum(value) - .5*value)
meltedAllDayAds$WDay <- factor(meltedAllDayAds$WDay, levels= c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
meltedAllDayAds[order(meltedAllDayAds$WDay), ]
ggplot(meltedAllDayAds, aes(x = WDay, y = value, fill = variable)) +
  geom_bar(stat = "identity", width = 0.75) +
  ggtitle("Composition of Pages as per Day of the Week (Average)") +
  ylab("Total Number of Pages") +
  xlab("Day of the Week") +
  scale_fill_manual(values = c("#424242", "#2E7D32"),  # Changing the colour of the bars
                    name = "Pages Composition",
                    breaks = c("Pages", "TotalPages"),
                    labels = c("Full Page Ads", "Pages with News")) + 
  geom_text(aes(y=label_y, label=value), vjust=0.5, color='white', fontface=1, size=5) +  # Putting the labels
  themefunc() +
  guides(fill = guide_legend(reverse=TRUE)) +  # To reverse the order of the legend
  ggsave('Days.png', dpi=400)

#####

# Companies mostly taking the front page

companiesFunction <- function(Variable)
{
  companies <- ns[Variable]  # Getting all the companies
  companies2 <- vector(mode = "character", length = 0)
  for (i in 1:nrow(companies))
  {
    print (companies[i, 1])
    companies2 <- c(companies2, strsplit(as.character(companies[i, 1]), ",", fixed = TRUE))
  }
  allCompanies <- vector(mode="numeric", 0)  # Empty Vector to store companies
  
  # Loop to get all the companies indivually 
  for (i in 1:length(companies2)){
    for (o in 1:length(companies2[[i]])){
      allCompanies <- c(allCompanies, str_trim(companies2[[i]][o]))
    }
  }
  
  allCompanies <- data.frame(allCompanies)  # Converting to data frame
  allCompanies <- plyr::count(allCompanies, "allCompanies")  # Counting the number of companies
  
  # Making a new variable Bar Graph
  cname <- vector(mode = "character", length=0)
  cvalue <- vector(mode = "integer", length=0)
  others <- vector(mode = "character", length=0)
  for (i in 1:nrow(allCompanies))
  {
    if (allCompanies[i, 2] <= 1)
    {
      others <- c(others, as.character(allCompanies[i, 1]))
    }
    else
    {
      cname <- c(cname, as.character(allCompanies[i, 1]))  # To put the company name
      cvalue[match(allCompanies[i, 1], cname)] = allCompanies[i, 2]  # To get the value of its occurance
    }
  }
  CompaniesCount <- data.frame(cname, cvalue)
  names(CompaniesCount) <- c("CompanyName", "Occurrence")  # Putting a name to the data frame
  # To put a line break in the company names
  return (list(CompaniesCount, others))  # Using list to return two values from this function
}


FrontPageComp <- companiesFunction("Front.Page.Ad.By")
write.csv(FrontPageComp[[2]], "Front Page Other CSV.csv")
FullPageComp <- companiesFunction("Full.Page.Ads.By")
write.csv(as.character(FullPageComp[[2]]), "Full Page Other CSV.csv")

#####Plotting of Companies#####

# Plotting for Companies which gives ads on Front Page
ggplot(data = FrontPageComp[[1]], aes(x = reorder(CompanyName, Occurrence), y = Occurrence, ymax=max(Occurrence))) + 
  scale_y_continuous(breaks = round(seq(0, max(30), by=2))) + 
  themefunc() + 
  xlab("Companies") +
  ylab("Number of times Ad Occurred") +
  ggtitle("Frequency of Companies Giving 1st Page Ads") +
  geom_text(aes(label=Occurrence), position=position_dodge(width=0.9), vjust=-0.4, size=5) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.1), width=0.69, fill="#2E7D32") +
  ggsave("FrontPage.png", dpi=400)


# Plotting for Companies which can give Full Page Ads
ggplot(data = FullPageComp[[1]], aes(x = reorder(CompanyName, -Occurrence), y = Occurrence, ymax=max(Occurrence))) + 
  scale_y_continuous(breaks = round(seq(0, max(50), by=3))) + 
  themefunc() + 
  xlab("Companies") +
  ylab("Number of times Ad Occurred") +
  ggtitle("Frequency of Companies Giving Full Page Ads") +
  geom_text(aes(label=Occurrence), position=position_dodge(width=0.9), vjust=0.5, size=4, hjust=-0.5) +
  geom_bar(stat = "identity", width=0.6, position = position_dodge(0.2), fill="#2E7D32") +
  coord_flip() +
  ggsave("FullPage.png", dpi=400)

#####Pie Chart for Multiple Front Page Ads#####

# Variables
multiple <- data.frame(plyr::count(ns$Multiple.Front.Pages)[1], round(plyr::count(ns$Multiple.Front.Pages)[2] / sum(plyr::count(ns$Multiple.Front.Pages)[2]) * 100, 0))
names(multiple) <- c("Pages", "Percentage")
multiple <- melt(multiple)
# Plotting the Pie Chart for Multiple Front Page Ads
ggplot(data = multiple, aes(x = variable, y = value, fill = Pages)) +
  geom_bar(stat = "identity", width = 0.09) +
  ylab("") +
  xlab("") +
  scale_fill_manual(values = c("#424242", "#2E7D32")) +
  geom_text(aes(y=c(20.5, 70), label=c("Multiple Front Page Ads Not Present\n 41%", "Multiple Front Page Ads Present\n 59%")), color='white', fontface=1, size=4.5) +  # Using Customized line here. Can be changed #
  theme(legend.position="none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.background = element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.title = element_text(vjust = -32.5, size = 16)) +  # Change the order of the legend
  coord_flip() +
  ggsave("Percentage.png", dpi = 400)
