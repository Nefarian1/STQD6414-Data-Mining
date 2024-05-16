set.seed(123)
n<-500
err<-rnorm(n,2)
m<-function(x){x*cos(x)}
X<-rnorm(n,3)
Y<-m(X)+err
plot(X,Y)

evalpoint<-seq(min(X),max(X),l=100)
evalpoint


###calculate K
normal<-function(evalpoint,x,h){
  sapply(x,function(x){(1/sqrt(2*pi*(0.37065^2)))*exp(-0.5*((evalpoint-x)/(0.37065*h))^2)})
}

ksmoothnormal<-function(x,X,Y,h){
  Kx<-normal(x,X,h)
  W<-Kx/rowSums(Kx)
  return(W%*%matrix(Y))
}

transformdata2<-ksmoothnormal(evalpoint,X,Y,5)
plot(X,Y)
lines(evalpoint,transformdata2,col=7,lwd=2)
lines(evalpoint,transformdata2,col=5,lwd=2)

####local ly weight regression
x<-c(1,2,4,7,9,10)
y<-c(10,15,17,19,20,22)
plot(x,y)
df<-data.frame(x=x,y=y)

####simple linear regression
model<-lm(y~x,data=df)
abline(model)

set.seed(123)
n<-500
err<-rnorm(n,2)
m<-function(x){x*cos(x)}
X<-rnorm(n,3)
Y<-m(X)+err
plot(X,Y)

evalpoint<-seq(min(X),max(X),l=4)

dataf<-data.frame(X=X,Y=Y)
lo<-loess(Y~X,data=dataf,span=2)
loout<-predict(lo,data.frame(X=evalpoint))
plot(X,Y)
points(evalpoint,loout,col=2)
lines(evalpoint,loout,col=3,lwd=3)

####srs
data<-seq(1,10,by=1)
data<-iris
sample(x=data,size=5,replace = TRUE)
sample(x=data,size=5,replace = FALSE)

set.seed(12)
sample.int(n=150,size=50)
reduceddata<-data[sample.int(n=150,size=50,replace=FALSE),]
dim(reduceddata)
dim(data)
table(reduceddata$Species)

idsetosa<-sample(x=which(data$Species=="setosa"),size=20)
idversicolor<-sample(x=which(data$Species=="versicolor"),size=20)
idvirginica<-sample(x=which(data$Species=="virginica"),size=20)
#check
idsetosa#one to fifty one
idversicolor#fifty one to hundred
idvirginica

randic<-c(idsetosa,idversicolor,idvirginica)
reduceddata<-data[randic,]
dim(reduceddata)
table(reduceddata$Species)

##twenty thirty thirty
data<-seq(1,10,by=1)
data<-iris
sample(x=data,size=5,replace = TRUE)
sample(x=data,size=5,replace = FALSE)

set.seed(12)
sample.int(n=150,size=50)
reduceddata<-data[sample.int(n=150,size=50,replace=FALSE),]
dim(reduceddata)
dim(data)
table(reduceddata$Species)

idsetosa<-sample(x=which(data$Species=="setosa"),size=20)
idversicolor<-sample(x=which(data$Species=="versicolor"),size=30)
idvirginica<-sample(x=which(data$Species=="virginica"),size=30)

idsetosa#one to fifty one
idversicolor#fifty one to hundred
idvirginica

randic<-c(idsetosa,idversicolor,idvirginica)
reduceddata<-data[randic,]
dim(reduceddata)
table(reduceddata$Species)


set.seed(1)
df<-data.frame(type=rep(c("Tuna","Dolphin","Whale","Salmon"),c(50,50,150,150)),weight=rnorm(400,mean=4,sd=2))
table(df$type)/400
###sample 120 observation using stratified sampling
idtuna<-sample(x=which(df$type=="Tuna"),size=15)
iddolphin<-sample(x=which(df$type=="Dolphin"),size=15)
idwhale<-sample(x=which(df$type=="Whale"),size=45)
idsalmon<-sample(x=which(df$type=="Salmon"),size=45)
randic<-c(idtuna,iddolphin,idwhale,idsalmon)
reduceddata<-data[randic,]
dim(reduceddata)
table(reduceddata$type)

mytable<-table(df$type)
namescat<-names(mytable)
noofsample<-110
sample_size<-round(noofsample*prop.table(mytable))
sampleid<-c()
for (i in 1:length(mytable)) {
  print(i)
  sampleid<-c(sampleid,sample(x=which(df$type==namescat[i]),size=sample_size[i]))
}
reduceddata<-df[sampleid,]
dim(reduceddata)
table(reduceddata$type)

###data aggregation
data<-iris
require(tidyverse)
data%>%group_by(Species)%>%summarize(ASepal.Length=mean(Sepal.Length),ASepal.Width=mean(Sepal.Width),
                                     APetal.Length=mean(Petal.Length),APetal.Width=mean(Petal.Width))
aggregate(.~Species,data=iris,mean)
type<-sample(c("A","B"),size=150,replace=TRUE)
newdata<-cbind(data,type)
aggregate(.~Species+type,data=newdata,mean)
newdata%>%group_by(Species,type)%>%summarize(ASepal.Length=mean(Sepal.Length),ASepal.Width=mean(Sepal.Width),
                                        APetal.Length=mean(Petal.Length),APetal.Width=mean(Petal.Width))

####parametric reduction:linear model
x<-c(1,2,4,7,9,10)
y<-c(10,15,17,19,20,22)
plot(x,y)
df<-data.frame(x=x,y=y)
model<-lm(y~x,data=df)
xnew<-seq(1:10)
ynew<-1.072*xnew+11.271
plot(xnew,ynew)

###PCA
data<-iris
data<-data[,1:4]
centered_data<-cbind(data[,1]-mean(data[,1]),data[,2]-mean(data[,2]),
                     data[,3]-mean(data[,3]),data[,4]-mean(data[,4]))

covX<-cov(centered_data)
eig<-eigen(covX)
V<-eig$vectors
exp_variation<-eig$values/sum(eig$values)
Y<-centered_data%*%V
cov(Y)
exp_variation

plot(1:length(exp_variation),exp_variation,type="l")
reduceddata<-Y[,1:2]
dim(reduceddata)
dim(data)

