#########################################################################################################################
# This file just performs explorartory data analysis on a dataset containing renewable energy projects around the world. 
# For access to the original data, visit http://ppi-re.worldbank.org/data 
# Or download the data straight from http://ppi-re.worldbank.org/~/media/GIAWB/RE/Documents/RE-data-all.xls
#########################################################################################################################



# Loading packages and setting the directory.

setwd("/Users/nonyeo./Documents/Spring 2019/DATA5100 - Statistical Analysis")

library(tidyverse)
library(dplyr)
library(plotly)
library(stargazer)
library(qdap)
library("ggpubr")
library(reshape)


# Loading csv dataset and viewing sample

data <- read.csv("RE_Projects.csv", na.strings = c("", "NA"), 
                 sep=",", header=TRUE, stringsAsFactors=FALSE)
data <- rename(data, c(Investments='Investments'))
colnames(data)
dim(data)
head(data)


# Summary table of select columns

df <- data.frame(data)
cols <- c('Investments', 'Capacity.MW.', 'PercentPrivate')
stargazer(
  df[, cols], type = "text", 
  summary.stat = c("min", "p25", "median", "mean", "p75", "max", "sd", "n")
)

# Investigating outliers

data[which.max(data$Investments), ]
data[which.max(data$Capacity.MW.), ]


# Bivariate, Regression & Correlation

ggplot(data, mapping = aes(x = Capacity.MW., y = Investments)) + geom_point() + 
  geom_smooth(method='lm') + 
  labs(x = "Project Capacity (MW)", y = "Investments") + 
  ggtitle("Relationship between Investments and Capacity") + 
  theme_bw()
cor.test(data$Capacity.MW., data$Investments)
ggplot(data, mapping = aes(x = PercentPrivate, y = Investments)) + geom_point() + 
  geom_smooth(method='lm') + 
  labs(x = "Asset Share", y = "Investments") + 
  ggtitle("Relationship between Investments and Asset Share") + 
  theme_bw()
cor.test(data$PercentPrivate, data$Investments)


#Hypothesis test for IDA Status and Technology

tbl = table(data$Technology, data$IncomeGroup) 
tbl
chisq.test(tbl)


# Summary totals of select attributes to better understand available elements (output hidden). 

lapply(data[,c(2,9,11,15,24)], table)
# Five countries with highest spend on RE projects
top5 <- tapply(data$Investments, INDEX=list(data$Country),FUN=sum, na.rm=T)
tail(sort(top5),5)


# Creating a full frequency table of regions represented in the dataset, and then a bar plot of the percentages.

freqRegion <- dist_tab(data$Region)
ggplot(freqRegion, aes(x=interval, y=percent)) + 
  geom_histogram(stat = "identity", fill = "darkslategray3", na.rm = T) +
  labs(x = "Region", y = "Share of Projects (%)", title = "Projects by Region") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

ggplot(data, aes(Technology)) + 
  geom_histogram(stat = "count", fill = "darkslategray3", na.rm = T) +
  theme_bw() +
  labs(x = "Types of Technology", y = "Projects", title = "Project Share by Type")

df <- data.frame(data$Technology, data$Investments)
freq <- tapply(df$data.Investments, INDEX=list(df$data.Technology),FUN=sum, na.rm=T)
freq1 <- aggregate(data.Investments~data.Technology,df,sum)
freqtotal <- freq1 %>% 
  mutate(PercentInvested = data.Investments / sum(data.Investments))
ggplot(freqtotal, aes(x=data.Technology, y=PercentInvested)) + 
  geom_histogram(stat = "identity", fill = "darkslategray3", na.rm = T) +
  labs(x = "Technology", y = "Share of Investments (%)", title = "Technology Investments") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

ggplot(data, aes(GovtGrantingContract)) + 
  geom_histogram(stat = "count", fill = "skyblue2", na.rm = T) +
  theme_bw() +
  labs(x = "Government Arms", y = "Contracts Granted", title = "Government Arms Granting Contracts")


# Univariate (Frequency distribution) of numerical columns

ggplot(data) +
  geom_histogram(mapping = aes(Investments), binwidth = 100, fill = "rosybrown", na.rm = TRUE) +
  theme_bw() +
  labs(x = "Project's Physical Assets ($m)", y = "Frequency")
ggplot(data) +
  geom_histogram(mapping = aes(Capacity.MW.), binwidth = 100, fill = "rosybrown", na.rm = TRUE) + 
  theme_bw() +
  labs(x = "Project Capacity", y = "Frequency")


# Piechart of project capacity per income group

totals <- aggregate(Capacity.MW.~IncomeGroup,data,length)
names(totals)[2] <- 'tallyincomegroup'
capacity <- aggregate(Capacity.MW.~IncomeGroup,data,sum)
names(capacity)[2] <- 'totalcapacity'
capincome <- merge(totals,capacity)
colours <- c("rgb(15, 145, 188)", "rgb(17, 155, 150)", "rgb(106, 114, 114)")

plot_ly(capincome, labels = ~IncomeGroup, values = ~totalcapacity, type = 'pie',
        textposition = 'inside', textinfo = 'label+text+percent', 
        insidetextfont = list(color = 'white'),  
        text = ~paste(totalcapacity, ' MW'), 
        marker = list(colors = colours, line = list(color = 'white', width = 1)), showlegend = FALSE) %>%
  layout(title = 'Capacity by Income Level', 
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), 
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


# Testing correlation between project capacity and investment (project assets)

cor.test(data$Capacity.MW., data$Investments,  method = "pearson", use = "complete.obs")
# Test if variables follow the normal distribution
plot(density(data$Capacity.MW., na.rm = TRUE))
plot(density(data$Investments, na.rm = TRUE))
shapiro.test(data$Capacity.MW.)
shapiro.test(data$Investments)
ggqqplot(data$Capacity.MW., ylab = "Capacity (MW)")
ggqqplot(data$Investments, ylab = "Project Assets")


# Boxplot

ggplot(data, aes(x = Region, y = Investments)) +
  geom_boxplot() + 
  scale_y_continuous(name = "Investments", 
                     breaks = c(0, 1000, 2000, 3000, 4000, 5000, 10000, 15000)) +
  labs(x = "Region", y = "Project's Physical Assets ($m)") +
  ggtitle("Asset Distribution by Region") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))


# Stacked Barplot

fill <- brewer.pal(9,"YlGnBu")
ggplot(data) + 
  geom_bar(aes(x = Region, y = Capacity.MW., fill = Technology), stat="identity") + 
  scale_fill_manual(values = fill) +
  ggtitle("Project Technology by Region") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 25, hjust = 1))



