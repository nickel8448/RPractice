library("ggplot2")
library("reshape2")
library("stringr")  # Library to strip the string
library("plyr")

ns <- read.csv("NS.csv")  # Reading the CSV

# Computing Days with Number of News Pages

newsPages <- data.frame(ns$Date, ns$Full.Page.Ads, I(ns$Total.Pages - ns$Full.Page.Ads))
newsHeading <- c("Date", "FullPageAds", "NewsPages")  # Renaming the columns
names(newsPages) <- newsHeading
newsPages2 <- melt(newsPages)
ggplot(data = newsPages2, aes(x = Date, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  ylab("Number of Pages") +
  scale_fill_manual(values = c("#E24139", "#158504"),
                    name = "Pages Composition",
                    breaks = c("FullPageAds", "NewsPages"),
                    labels = c("Full Page Ads", "Pages with News")) +
  theme(panel.background = element_blank()) + # No background
  coord_flip()

# Companies mostly taking the front page

companies <- ns$Front.Page.Ad.By  # Getting all the companies
companies2 <- strsplit(as.character(companies), ",", fixed=TRUE)  # Splitting the Companies

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
    cname <- c(cname, as.character(allCompanies[i, 1]))  # To get the company name
    cvalue[match(allCompanies[i, 1], cname)] = allCompanies[i, 2]  # To get the value of its occurance
  }
}

CompaniesCount <- data.frame(cname, cvalue)
names(CompaniesCount) <- c("CompanyName", "Occurrence")

ggplot(data = CompaniesCount, aes(x = CompanyName, y = Occurrence)) + 
  scale_y_continuous(breaks = round(seq(0, max(15), by=2))) + 
  theme(panel.border=element_blank(), 
        panel.background=element_blank()) + 
  geom_bar(stat = "identity")

