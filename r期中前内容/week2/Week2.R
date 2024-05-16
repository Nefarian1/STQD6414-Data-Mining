install.packages("dplyr")
install.packages("tidyr")

setwd("D:/Rstudio/R project/week2 exercise")#路径
data<-read.csv("data_structuralerror.csv",header=TRUE,skip=4,strip.white=TRUE,
               na.strings = c("N/A","NA","-",""))

library(tidyr)
library(dplyr)
names(data)<-tolower(names(data))
data$dishwasher<-gsub("[[:punct:]]","",data$dishwasher)
data$dishwasher<-tolower(data$dishwasher)
data$no.of.mugs.plates<-gsub("[^0-9,-]","",data$no.of.mugs.plates)
data$dishwasher<-tolower(data$dishwasher)

data<-read.csv("data_structuralerror.csv",header=TRUE,skip=4,strip.white=TRUE,
               na.strings = c("N/A","NA","-",""))
install.packages("tidyverse")
require(tidyverse)
data<-data%>%rename_with(str_to_lower)
data<-data%>%mutate(dishwasher=str_replace_all(data$dishwasher,"[[:punct:]]",""))
data<-data%>%mutate(dishwasher=str_to_lower(data$dishwasher))
data<-data%>%mutate(no.of.mugs.plates=str_replace_all(data$no.of.mugs.plates,"[^0-9,-]",""))
data<-data%>%distinct()

cleandate<-pmax(
strptime(data$date,format = "%d/%m/%y"),
strptime(data$date,format = "%d-%m-%y"),
strptime(data$date,format = "%d-%m-%Y"),
strptime(data$date,format = "%d/%m/%Y"),
strptime(data$date,format = "%d/%b/%y"),
strptime(data$date,format = "%d %b %Y"),
strptime(data$date,format = "%d %b %y"),na.rm=TRUE)

#create a new variable in your data called clean date using the cleandate that we just create
data<-data%>%mutate(clean.date=as.Date(cleandate))

install.packages("lubridate")
##short cut
require(lubridate)
require(tidyverse)
data<-data%>%mutate(clean.date=parse_date_time(date,"dmy"))
data<-data%>%mutate(sink=case_when(str_detect(sink.table,"sink|both")==TRUE~"yes",
                                   is.na(sink.table)==TRUE~NA,.default = "no"))
data<-data%>%mutate(table=case_when(str_detect(sink.table,"table|both")==TRUE~"yes",
                                   is.na(sink.table)==TRUE~NA,.default = "no"))

data<-data%>%separate(no.of.mugs.plates,c("lower","upper"),"-")
data<-data%>%mutate(no.of.mugs.plates=case_when(is.na(upper)~as.numeric(lower)+as.numeric(upper)/2),
                    .default=as.numeric(lower))
data<-data%>%mutate(no.of.mugs.plates=case_when(!is.na(upper)~as.numeric(lower)+as.numeric(upper)/2),
                    .default=as.numeric(lower))
data<-data%>%mutate(no.of.mugs.plates=case_when(!is.na(upper)~(as.numeric(lower)+as.numeric(upper)/2),
                    .default=as.numeric(lower)))

##select column, retain only clean.date,name,sink,table,no.of.mugs.plates,note
data<-data%>%select(clean.date,name,sink,table,no.of.mugs.plates,note)

load("data_missing.Rda")

originaldata<-missingdata<-missing
sum(is.na(originaldata))

##introduce missing value
set.seed(123)
missingdata[sample(1:nrow(missingdata),100),1]<-NA
missingdata[sample(1:nrow(missingdata),100),3]<-NA

sum(is.na(missingdata))

newdata<-na.omit(missingdata)
newdata2<-missingdata%>%drop_na()
newdata3<-missingdata[complete.cases(missingdata),]

###remove column
colSums(is.na(missingdata))
newdata4<-missingdata[,c(2,4,5)]
newdata5<-missingdata%>%select(V2,V4,V5)

####impute using mean,median,mode
imputedata<-missingdata
imputedata$V1[is.na(missingdata$V1)]<-mean(missingdata$V1,na.rm = TRUE)
colSums(is.na(imputedata))

imputedata$V1[is.na(missingdata$V1)]<-mean(missingdata$V1,na.rm = TRUE)

table(missingdata$V3)
imputedata$V3[is.na(missingdata$V3)]<-3

####impute by knn
install.packages("VIM")
imputeknn<-VIM::kNN(missingdata)
imputeknn<-imputeknn[,1:5]

###impute by mice
install.packages("mice")
micemodel<-mice::mice(missingdata)
imputemice<-mice::complete(micemodel)
sum(is.na(imputemice))

##impute regression
model<-lm(V1~V2,data = missingdata)
new.df<-data.frame(missingdata[is.na(missingdata$V1),2])
names(new.df)<-"V2"
imputedata$V1[is.na(missingdata$V1)]<-predict(model,new.df)

######outlier
require(readr)
data<-read_tsv("data_outlier.txt")
datadf<-as.data.frame(data)

out<-boxplot(datadf$hwy)$out
which(datadf$hwy %in% out)

install.packages("outliers")
outliers::grubbs.test(datadf$hwy)
outliers::grubbs.test(datadf$hwy,opposite = TRUE)

install.packages("EnvStats")
EnvStats::rosnerTest(datadf$displ,k=5)

model<-lm(V1~V2,data = missingdata)
influence.measures(model)
