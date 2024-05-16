#Mining Web Data

#check wheater the website allow you to scrap thier data
install.packages("robotstxt")
library(robotstxt)
paths_allowed(paths=c("https://en.wikipedia.org/wiki/List_of_Formula_One_drivers"))

#import the data
library(xml2)
library(rvest)

link<-"https://en.wikipedia.org/wiki/List_of_Formula_One_drivers"
page<-read_html(link)

#extract table
drivers_F1<-html_element(page,"table.sortable")%>%
  html_table()

head(drivers_F1)
tail(drivers_F1)

#Data cleaning
#select some variables
drivers_F1i<-drivers_F1[c(1:4,7:9)]

#remove last row
drivers_F1i<-drivers_F1i[-868, ]

drivers_F1i$"Drivers' Championships"<-substr(drivers_F1i$"Drivers' Championships",
                                             start = 1,stop = 0)

#Which country has the largest number of wins
library(tidyverse)
drivers_F1i%>%group_by(Nationality)%>%
summarise(championship_C=sum(as.double(`Drivers' Championships` )))
arrange(desc(championship_C))

#Which driver has the most championship
drivers_F1i%>%group_by(`Driver name`)%>%
summarise(championship_pilot=sum(as.double(`Drivers' Championships` )))%>%
arrange(desc(championship_pilot)) 

#is there a relation between the number of championships won and
#the number of race pole position?
library(ggplot2)
drivers_F1i%>%filter(`Pole positions`>1)%>%
ggplot(aes(x=as.double(`Pole positions`),y=as.double(`Drivers' Championships`)))+
geom_point(position = "jitter")

#Data: World population
#check whether the website allow you to scrap thier data
library(robotstxt)
paths_allowed(paths=c("https://en.wikipedia.org/wiki/World_population"))

#import the data
url<-"https://en.wikipedia.org/wiki/World_population"
link<-read_html(url)

#extract all the paragraphs
paragraphs<-read_html(url)%>%html_nodes("p")%>%html_text()

#the website contain multiple table
Table1<-link%>%html_nodes("table")%>%`[[`(1)%>%html_table()
Table2<-link%>%html_nodes("table")%>%`[[`(2)%>%html_table()
Table4<-link%>%html_nodes("table")%>%`[[`(4)%>%html_table()
Table5<-link%>%html_nodes("table")%>%`[[`(5)%>%html_table()






