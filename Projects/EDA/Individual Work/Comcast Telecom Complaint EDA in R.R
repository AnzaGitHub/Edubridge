#DESCRIPTION OF DATA

#Comcast is an American global telecommunication company. The firm has been providing terrible customer service. They continue to fall short despite repeated promises to improve. Only last month (October 2016) the authority fined them a $2.3 million, after receiving over 1000 consumer complaints.

#The existing database will serve as a repository of public customer complaints filed against Comcast. It will help to pin down what is wrong with Comcast's customer service.

#Tasks Which We Will Be Performing :
  
  #* Import data into R environment.

  #* Provide the trend chart for the number of complaints at monthly and daily granularity levels.

  #* Provide a table with the frequency of complaint types.

  #* Which complaint types are maximum i.e., around internet, network issues, or across any other domains.

  #* Create a new categorical variable with value as Open and Closed. Open & Pending is to be categorized as Open and Closed & Solved is to be categorized as Closed.

  #* Provide state wise status of complaints in a stacked bar chart. Use the categorized variable from Q3. Provide insights on:
  
  #* Which state has the maximum complaints Which state has the highest percentage of unresolved complaints

  #* Provide the percentage of complaints resolved till date, which were received through theInternet and customer care calls.

  #* The analysis results to be provided with insights wherever applicable.
library(readxl)
library(lubridate) #Use Lubridate Library to Format the Date Column
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)
library(plyr)
library(gridExtra)

Data <- read.csv("D:/Data Analytics/EDA/Comcast Telecom Complaints data.csv")
head(Data,4) #Printing Few Rows Of Data 
str(Data) #checking the structure of the dataset.

#Loading The Date Into Single Format


li<-parse_date_time(x = Data$Date,
                    orders = c("d m y", "d B Y", "m/d/y"),
                    locale = Sys.getlocale("LC_TIME"))
data2<-Data
data2$Date <- li
#Dates Loaded In the Same Format in the new Dataframe
#str(data2$Date)

#Extracting Month Column and Converting to The labels. 
data2$Month <- format(as.Date(data2$Date), "%m")
data2$Month<- month.abb[as.integer(data2$Month)]
head(data2)


data_date<-data2 %>% group_by(Date) %>% dplyr::summarise(frequency = n())
df <-data_date[order(-data_date$frequency),]
dff<-head(df)
dff


ggplot(data_date, aes(Date, frequency, group = 1)) + 
  geom_point() + 
  geom_line() +
  xlab("Date") + 
  ylab("Number of Complaints")


ggplot(dff, aes(Date, frequency, group = 1)) + 
  geom_point() + 
  geom_line() +
  xlab("Date") + 
  ylab("Number of Complaints")

data_month<-data2 %>% 
  group_by(Month) %>% dplyr :: summarise(frequency = n())
data_month

data2$Month <- as.factor(data2$Month)
levels(data2$Month)


ggplot(data_month, aes(Month, frequency, group = 1)) + 
  geom_point() + 
  geom_line() +
  xlab("Month") + 
  ylab("Number of Complaints")

#You only have to add group = 1 into the ggplot or geom_line aes().
#For line graphs, the data points must be grouped so that it knows which points to connect. 
#In this case, it is simple -- all points should be connected, so group=1. 
#When more variables are used and multiple lines are drawn, the grouping for lines is usually done by variable.

#Fetching The Top 20 complaints filed by customers on different days.
final_most<-head(final,20)
final_most

#Fetching The Top 20 complaints filed by customers on different days.
final_most<-head(final,20)
final_most


ggplot(head(final_most,6), aes(CustomerComplaintType, Frequency)) +
  geom_bar(stat = "identity")


levels(Data$Status)


Data$Status_New<-revalue(Data$Status, c(Pending = "Open", Solved = "Closed"))
head(Data)

levels(Data$State)

tab <- table(Data$State,Data$Status_New)
tab <- cbind(tab, Total = rowSums(tab))
head(tab,15)


ggplot(Data, aes(y = State)) + geom_bar(aes(fill = Status_New)) 

levels(Data$Received.Via)

ggplot(Data, aes(y = Received.Via )) + geom_bar(aes(fill = Status_New)) 

df1 <- table(Data$Received.Via, Data$Status_New)
df1 <- cbind(df1, Total = rowSums(df1))
df1


# Pie Chart with Percentages
slices <- c(864, 255)
lbls <- c("Closed", "Open")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Received Via Call") 

# Pie Chart with Percentages
slices <- c(843, 262)
lbls <- c("Closed", "Open")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Received Via Internet") 

#Solutions -
  #* The Company should Focus more on resolving complaints - Customer Are Mainly complaining about the Data Caps, Internet Speed, Billing Methods and Services that Comcast is Providing and Very few Cases were registered against Comcast Cable Services.

  #* In Georgia and Florida company services are already Improving but, in States - California, Colorado and Illinois company should extend their resources in terms of the above-mentioned issues in order to improve their customer servicing.

  #* During the month of June and the start of July, the Company reported lots of complaints, so as to for future reference they can keep this in check already so as to provide better services during these months. While working with their BPO clients to extend the staff during such days. Which ensures proper feedback for the particular arisen issue.
