airquality
str(airquality)
require(tidyverse)
airdata<-airquality%>%select(Day,Month,Temp,Wind)
airdata57<-airquality%>%select(Day,Month,Temp,Wind)%>%filter(Month==5|Month==6|Month==7)
airdata57<-airquality%>%select(Day,Month,Temp,Wind)%>%filter(Month!=8|Month!=9)

airdata69<-airquality%>%select(-Temp)%>%filter(Month!=5)

airmerge<-full_join(airdata57,airdata69)
airmerge2<-merge(airdata57,airdata69,all=TRUE)
airmerge2%>%arrange(Month)

#####
airdata69[1,]
airdata69[1,3]<-9999

airdata57[32,]

airmergew<-full_join(airdata57,airdata69)
airmerge2w<-merge(airdata57,airdata69,all=TRUE)
str(airmergew)
#####
set.seed(0)
dataleft<-data.frame(id=c(1:10),height=rnorm(10,170,5))
dataright<-data.frame(id=rep(5:8,each=2),alpha=sample(letters,8))
dataright
full_join(dataleft,dataright)
merge(dataleft,dataright,all=TRUE)




inner_join(dataleft,dataright)
merge(dataleft,dataright,all.x=TRUE)

left_join(dataleft,dataright)
merge(dataleft,dataright,all.y=TRUE)

names(dataleft)[1]<-"ids"

full_join(dataleft,dataright,by=c("ids"="id"))
merge(dataleft,dataright,all=TRUE,by.x="ids",by.y="id")

##appending rows
names(dataleft)[1]<-"id"
set.seed(0)
dataleft<-dataleft%>%mutate("weight"=rnorm(10,60,3))

bind_rows(dataleft,dataright)
rbind(dataleft,dataright)

bind_cols(dataleft,dataright)
cbind(dataleft,dataright)

set.seed(0)
dataleft2<-data.frame(id=c(1:10),height=rnorm(10,160,5))
bind_cols(dataleft,dataleft2)
cbind(dataleft,dataleft2)

#######(week3)
dat1<-read.csv("Set1_emp1.csv",skip=1,header=TRUE,strip.white = TRUE)
names(dat1)[7]<-"salary(MYR)"

dat2<-read.csv("Set1_emp2.csv",skip=2,header=TRUE,strip.white = TRUE)
names(dat2)[6]<-"salary(USD)"

dat1<-dat1%>%unite("name",first.name:last.name)
dat1$name<-dat1$name%>%str_replace("_"," ")

fulldata<-full_join(dat1,dat2)
fulldata[-7,]
fulldata%>%filter(status=="active")

#######data transformation
#######min-max normalization
set.seed(0)
data<-cbind(rnorm(1000,3,30),rnorm(1000,4,2))
minmax<-function(x){
  out<-(x-min(x))/(max(x)-min(x))
  return(out)
}
transformdata<-cbind(minmax(data[,1]),minmax(data[,2]))
######
###z-score
set.seed(0)
data<-cbind(rnorm(1000,3,30),rnorm(1000,4,2))
zscore<-function(x){
  out<-(x-min(x))/(max(x)-min(x))
  return(out)
}
transformdata<-cbind(zscore(data[,1]),zscore(data[,2]))
transformdata2<-apply(data,2,zscore)
transformdata3<-scale(data)
plot(transformdata)
plot(data)

#####
set.seed(0)
data<-cbind(rnorm(1000,3,30),rnorm(1000,4,2))

zscore<-function(x){
  maxx<-max(abs(x))
  j<-floor(log10(maxx))+1
    out<-x/(10^j)
  return(out)
}
log10(500)
10^2.69897
floor(log10(500)+1)
######log10
transformdata<-apply(data,2,decimalscale)
summary(transformdata)
summary(data)

floor(3.5)
ceiling(3.5)
#########square root/log/inverse transformation(week2)
data<-read.csv("Set1_trans.csv")

hist(data[,1])
hist(data[,2])
hist(data[,3])
hist(data[,4])
hist(data[,5])
hist(data[,6])
summary(data)
hist(log(data[,1]+1))
hist(log(-data[,2]+1))
hist(1/-data[,2])
hist(sqrt(data[,3]))
hist(sqrt(-data[,4]))
hist(log(data[,5]+22))
hist(1/-data[,6])
#######
rbintrans<-function(x,k=3/8){
  n<-length(x)
  xr<-rank(x)
  out<-qnorm((xr-k)/(n-2*k+1))
  return(out)
}
transformdata<-apply(data,2,rbintrans)
plot(transformdata)
plot(data)

data<-read.csv("Set1_trans.csv")
hist(rbintrans(data[,2]))
hist(data[,1])
########box-cox
x<-data[,1]+1
out<-MASS::boxcox(lm(x~1))
lambda<-out$x[which.max(out$y)]

boxcox<-function(x){
  out<-MASS::boxcox(lm(x~1))
  lambda<-out$x[which.max(out$y)]
  if(lambda==0){output<-log(x)}
  else{output<-(x^lambda-1)/lambda}
  return(output)
}
transformdata<-apply(data,2,boxcox)
plot(transformdata)
plot(data)

head(transformdata)
