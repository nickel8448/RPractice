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
          axis.text.x = element_text(size = 11, colour="black"),
          axis.text.y = element_text(size = 11, colour="black"),
          axis.title.y = element_text(size = 14, colour="black", vjust = 1.5),
          axis.title.x = element_text(size = 14, colour="black", vjust=-.5)) # No background
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
  scale_fill_manual(values = c("#E24139", "#158504"),  # Chaning the colour of the bars
                    name = "Pages Composition",  # Name of the Graph
                    breaks = c("FullPageAds", "NewsPages"),  # Name of the legend
                    labels = c("Full Page Ads", "Pages with News")) +  # Changing the legend
  scale_y_continuous(breaks = round(seq(0, max(30), by=5))) +
  geom_text(aes(y=label_y, label=value), vjust=0.5, color='white', fontface=1, size=3) +  # Putting the labels
  themefunc() +
  coord_flip() +  # Flipping the graph
  ggsave("Ads.png", dpi=300) 

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
ggplot(meltedAllDayAds, aes(x = WDay, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  ylab("Total Pages") +
  xlab("Day of the Week") +
  scale_fill_manual(values = c("#E24139", "#158504"),  # Changing the colour of the bars
                    name = "Pages Composition",
                    breaks = c("Pages", "TotalPages"),
                    labels = c("Full Page Ads", "Pages with News")) + 
  geom_text(aes(y=label_y, label=value), vjust=0.5, color='white', fontface=1, size=5) +  # Putting the labels
  themefunc() +
  ggsave('Days.png', dpi=300)

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
  allCompanies <- count(allCompanies, "allCompanies")  # Counting the number of companies

  # Making a new variable Bar Graph
  cname <- vector(mode = "character", length=0)
  cvalue <- vector(mode = "integer", length=0)
  for (i in 1:nrow(allCompanies))
  {
    if (allCompanies[i, 2] <= 1)
    {
      if (!('Others' %in% cname))
      {
        cname <- c(cname, 'Others')
        cvalue[match('Others', cname)] = 1
      }
      else
      {
        cvalue[match('Others', cname)] = I(cvalue[match('Others', cname)] + 1)
      }
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
  return (CompaniesCount)
  
}

FrontPageComp <- companiesFunction("Front.Page.Ad.By")
FullPageComp <- companiesFunction("Full.Page.Ads.By")

#####Plotting of Companies#####
levels(FrontPageComp$CompanyName) <- as.character(gsub(" ", "\n", levels(FrontPageComp$CompanyName)))

# Plotting for Companies which gives ads on Front Page
ggplot(data = FrontPageComp, aes(x = reorder(CompanyName, Occurrence), y = Occurrence, ymax=max(Occurrence))) + 
    scale_y_continuous(breaks = round(seq(0, max(30), by=2))) + 
    themefunc() + 
    xlab("Companies") +
    ylab("Number of times Ad Occurred") +
    ggtitle("Frequency of various Companies occurring on 1st Page") +
    geom_text(aes(label=Occurrence), position=position_dodge(width=0.9), vjust=-0.4, size=4) +
    geom_bar(stat = "identity", width=0.5, position = position_dodge(width=1), fill="#158504") +
  ggsave("FrontPage.png", dpi=300)


# Plotting for Companies which can give Full Page Ads
ggplot(data = FullPageComp, aes(x = reorder(CompanyName, -Occurrence), y = Occurrence, ymax=max(Occurrence))) + 
    scale_y_continuous(breaks = round(seq(0, max(50), by=2))) + 
    themefunc() + 
    xlab("Companies") +
    ylab("Number of times Ad Occurred") +
    ggtitle("Frequency of various Companies Giving Full Page Ads") +
    geom_text(aes(label=Occurrence), position=position_dodge(width=0.9), vjust=0.5, size=4, hjust=-0.5) +
    geom_bar(stat = "identity", width=0.5, position = position_dodge(0.7), fill="#158504") +
    coord_flip() +
    ggsave("FullPage.png", dpi=300)

#####Pie Chart for Multiple Front Page Ads#####

# Variables
multiple <- data.frame(ns$Date, ns$Multiple.Front.Pages, ns$Total.Pages)
names(multiple) <- c("Date", "Multiple Front Pages", "Total Pages")
multiple$`Multiple Front Pages` <- factor(multiple$`Multiple Front Pages`, levels = rev(levels(multiple$`Multiple Front Pages`)))  # Changing the order of the levels
multiple <- multiple %>% group_by(multiple$Date) %>% mutate(pos = cumsum(multiple$`Multiple Front Pages`)- multiple$`Multiple Front Pages`/2)

# Plotting the Pie Chart for Multiple Front Page Ads
ggplot(data = multiple, aes(x = factor(1), fill=multiple$`Multiple Front Pages`)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  xlab("") + 
  ylab("") +
  scale_fill_manual(values = c("#E24139", "#158504"),
                    name = "",  # Name of the Graph
                    breaks = c("No", "Yes"),  # Name of the legend
                    labels = c("Multiple Front Page Not Present", " Multiple Front Pages with Ads Present")) + 
  scale_y_continuous(breaks = round(seq(0, max(0)))) +
  guides(fill = guide_legend(reverse=TRUE)) +  # Change the order of the legend
  scale_x_discrete(breaks=NULL) +  # Remove the 
  themefunc()
