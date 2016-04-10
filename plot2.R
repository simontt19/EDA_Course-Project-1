
setwd("D:/01-PRIVATE/workspace/r/exploratory")
rm(list=ls())

# ==================================================
#   Description:
#   Electric power consumption data plotting -plot2
# 
# History:
#   1.00  2016-04-10  Simon  Creation
# ==================================================

# 1. download and unzip file
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip","household_power_consumption.zip")
unzip("household_power_consumption.zip", exdir = "D:/01-PRIVATE/workspace/r/exploratory/data")

# 2. load data
data<-read.table("./data/household_power_consumption.txt",header=TRUE)

# 3. cleaning data

## 3.1 split columns name and save in a variable
colnames<-strsplit(names(data),"\\.")

## 3.2 change columns name
names(data)<-"X"

## 3.3 split data into a list
data_list<-strsplit(as.character(data$X),"\\;")

## 3.4 loop through list to create data frame (condition 2-day period in February, 2007)
n=data.frame(t(data_list[[1]]))
for(i in 2:length(data_list)){
  date<-as.character(t(data_list[[i]])[1,1])
  if(date=="1/2/2007" | date=="2/2/2007"){
    n=rbind(n,data.frame(t(data_list[[i]])))}
}

data_f<-n[n[,1]!='16/12/2006',]

## 3.5 change data measurement into double
l=data_f[,1:2]
for(i in 3:9){
  l<-cbind(l,as.double(as.character(data_f[,i])))
}

## 3.6 rename column
data_l<-l
names(data_l)<-colnames[[1]]

# 4. plot graph
library(lubridate)
library(timeDate)

## 4.1 plot graph on screen
Sys.setlocale("LC_TIME", "USA")
with(data_l,plot(Global_active_power ~ timeDate(dmy(Date)+hms(Time))@Data,type="l",xlab = "",ylab="Global Active Power (kilowatts)"))
## 4.2 copy and save screen file to png
dev.copy(png,file="plot2.png")
dev.off()
