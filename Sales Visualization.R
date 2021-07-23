r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)


SalesData = read.csv("D:/praneeta/praneeta/R/R case study 3 (Visualization)/SalesData.csv")
library(dplyr)
library(ggplot2)
install.packages("reshape2")
library(reshape2)
install.packages("tidyr")
library(tidyr)
View(SalesData)
#question1
summ_data = SalesData%>%
  group_by(Region)%>%
  summarise(TotalSales2015 = sum(Sales2015),TotalSales2016 = sum(Sales2016))
summ_data
data_long = gather(summ_data,key = Year, value = Sales,2:3)
data_long
data_long$Sales <- round(data_long$Sales,0)
ggplot(data_long,aes(Region,Sales, fill = Year,label = Sales)) + geom_bar(stat = "identity",position = "dodge") + 
  geom_text(size = 4) + xlab('Region') +ylab('Sales') + ggtitle("Comparision of Sales by Region")

#question2
sales2016 <- SalesData%>%
  group_by(Region)%>%
  summarise(TotalSales2016 = sum(Sales2016))
value_percent <- sales2016$TotalSales2016/sum(sales2016$TotalSales2016)*100
pie(sales2016$TotalSales2016,labels = paste(sales2016$Region,":",round(value_percent,1),"%",sep = " "),main = "Sales by Regions in 2016")

install.packages("plotrix")
library(plotrix)
pie3D(sales2016$TotalSales2016,explode=0.1,labels = paste(sales2016$Region,":",round(value_percent,1),"%",sep = " "),main = "Sales by Regions in 2016")

#question 3
rt_data <- SalesData%>%
  group_by(Region,Tier)%>%
  summarise(TotalSales2015 = sum(Sales2015),TotalSales2016 = sum(Sales2016))

data_long1 <- gather(rt_data,key = Year,value = Sales,-c(Region,Tier))
data_long1
ggplot(data_long1,aes(Tier,Sales,fill = Year)) + geom_bar(stat = "identity", position = "dodge") + facet_wrap(~ Region) +ggtitle("Comparision of Sales by Region and Tiers in both years")

#question4
state_data <- SalesData %>%
  group_by(State)%>%
  filter(Region =="East")%>%
  summarise(TotalSales2015 = sum(Sales2015),TotalSales2016 = sum(Sales2016))

data_long2 <- gather(state_data,key=Year,value=Sales,-State)
data_long2

ggplot(data_long2,aes(State,Sales,fill=Year)) + geom_bar(stat="Identity",position = "dodge") +ggtitle("Statewise Sales in the East Region")

#question5
hightierdata <- SalesData %>%
  group_by(Division) %>%
  filter(Tier == "High") %>%
  summarise(TotalUnits2015 = sum(Units2015), TotalUnits2016 = sum(Units2016))

data_long3 <- gather(hightierdata,key=Year,value=Units,-Division)
ggplot(data_long3, aes(Division,Units,fill=Year)) + geom_bar(stat = "Identity", position="dodge") +ggtitle("Units sold in high tier") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#question 6

SalesData$Qtr = case_when(
    SalesData$Month == "Jan" | SalesData$Month == "Feb" | SalesData$Month == "Mar" ~ "Q1",
    SalesData$Month == "Apr" | SalesData$Month == "May" | SalesData$Month == "Jun" ~ "Q2",
    SalesData$Month == "Jul" | SalesData$Month == "Aug" | SalesData$Month == "Sep" ~ "Q3",
    TRUE ~ "Q4"
  )

#question 7
qtr_data <- SalesData%>%
  group_by(Qtr)%>%
  summarise(TotalSales2015 = sum(Sales2015),TotalSales2016 = sum(Sales2016))
qtr_data
data_long4 = gather(qtr_data,key = Year, value = Sales,-Qtr)
data_long4
ggplot(data_long4,aes(Qtr,Sales, fill = Year)) + geom_bar(stat = "identity",position = "dodge") + xlab('Quarter') +ylab('Sales') + 
  ggtitle("Comparision of Sales by Quarter")

#question 8
data_q1 = SalesData%>%group_by(Qtr,Tier)%>%filter(Qtr=="Q1")%>%summarise(TotalSales2015 = sum(Sales2015))
data_q2 = SalesData%>%group_by(Qtr,Tier)%>%filter(Qtr=="Q2")%>%summarise(TotalSales2015 = sum(Sales2015))
data_q3 = SalesData%>%group_by(Qtr,Tier)%>%filter(Qtr=="Q3")%>%summarise(TotalSales2015 = sum(Sales2015))
data_q4 = SalesData%>%group_by(Qtr,Tier)%>%filter(Qtr=="Q4")%>%summarise(TotalSales2015 = sum(Sales2015))

par(mfrow=c(2,2))
par("mar")
par(mar=c(1,1,1,1))

value_percent1 <- data_q1$TotalSales2015/sum(data_q1$TotalSales2015)*100
pie(data_q1$TotalSales2015,labels = paste(data_q1$Tier,":",round(value_percent1,1),"%",sep = " "),main = "Qtr 1")
value_percent2 <- data_q2$TotalSales2015/sum(data_q2$TotalSales2015)*100
pie(data_q2$TotalSales2015,labels = paste(data_q2$Tier,":",round(value_percent2,1),"%",sep = " "),main = "Qtr 2")
value_percent3 <- data_q3$TotalSales2015/sum(data_q3$TotalSales2015)*100
pie(data_q3$TotalSales2015,labels = paste(data_q3$Tier,":",round(value_percent3,1),"%",sep = " "),main = "Qtr 3")
value_percent4 <- data_q4$TotalSales2015/sum(data_q4$TotalSales2015)*100
pie(data_q4$TotalSales2015,labels = paste(data_q4$Tier,":",round(value_percent4,1),"%",sep = " "),main = "Qtr 4")

