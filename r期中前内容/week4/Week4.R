set.seed(123)
data<-rnorm(1000,0,3)
hist(data)
head(data)
  
equalwidth<-function(x,bin){
minx<-max(x)
maxx<-min(x)
width<-(maxx-minx)/bin
return(cut(x,breaks=seq(minx,maxx,width),include.lowest=TRUE))
}
  
?cut
?seq
  
transformdata<-equalwidth(data,4)
str(transformdata)
str(data)
table(transformdata)
barplot(table(transformdata))  
head(transformdata)

discdata<-as.numeric(transformdata)
table(discdata)

set.seed(123)
data<-rnorm(1000,0,3)

####equal frequency discretization
equalfreq<-function(x,bin){
  n<-length(x)
  freq<-n/bin
  id<-c(1,seq(0,n-1,freq)[-1],n)
  breaksid<-sort(x)[id]
  return(cut(x,breaks=breaksid,include.lowest=TRUE))
}

transformdata<-equalfreq(data,4)
table(transformdata)

####clustering by kmeans
set.seed(123)
data<-rnorm(1000,0,3)

out<-kmeans(data,5)

transformdata<-out$cluster
#we lose the ordering of the data. so you need to examin the cluster center.
transformdatanew<-transformdata
transformdatanew[transformdata==3]<-1
transformdatanew[transformdata==1]<-2
transformdatanew[transformdata==4]<-3
transformdatanew[transformdata==5]<-4
transformdatanew[transformdata==2]<-5

table(transformdata)
table(transformdatanew)


####supervised
install.packages("discretization")
qchisq(p=0.1,1,lower.tail = FALSE)

x<-c(1,3,7,8,9,11,23,37,39,45,46,59)
y<-c(1,2,1,1,1,2,2,1,2,1,1,1)
out<-discretization::chiM(cbind(x,as.factor(y)),alpha=0.1)
transformdata<-out$Disc.data[,1]
out

x<-c(1,3,7,8,9,11,23,37,39,45,46,59)
y<-c(1,2,1,1,1,2,2,1,2,1,1,1)
out<-discretization::chi2(cbind(x,as.factor(y)),alp=0.5,del=0.001)
transformdata<-out$Disc.data[,1]
out

####
set.seed(123)
n<-500
err<-rnorm(n,2)
m<-function(x){x*sin(x)}
X<-rnorm(n,3)
Y<-m(X)+err
plot(X,Y)

evalpoint<-seq(min(X),max(X),l=100)
evalpoint

box<-function(evalpoint,x,h){
  sapply(x,function(x){ifelse(((evalpoint-x)/h)>=-0.5&((evalpoint-x)/h)<=0.5,1,0)})
}
box(evalpoint,X,2)

ksmoothbox<-function(x,X,Y,h){
  Kx<-box(x,X,h)
  W<-Kx/rowSums(Kx)
  return(W%*%matrix(Y))
}
transformdata<-ksmoothbox(evalpoint,X,Y,10)
plot(X,Y)
lines(evalpoint,transformdata,col=2,lwd=5)


dim(box(evalpoint,X,2))
(evalpoint-x)/h>=-0.5




