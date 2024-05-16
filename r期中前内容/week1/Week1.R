myarray<-array(1:12,c(2,3,2))
myarray

amfrix<-matrix<-matrix(1:6,nrow=2,ncol=3)
amfrix

mymatrix<-c(1:6,nrow=2,ncol=3)
mymatrix

myarray<-array(1:12,c(2,3,2))
myarray

avec<-c(1,2,3,4)
avec
vec2<-c("hello","good","day")
vec2
vec<-seq(1,2,3)
vec
vec<-seq(1,10)
vec

typeof(vec)
vec[5]
vec[5]<-10
vec
vec[5]<-"test"
vec
typeof(vec)

testm<-matrix(vec,nrow=2,ncol=5)
testm

test<-matrix(1:6,nrow=2,ncol=3)
test
testdf<-as.data.frame(test)
testdf
typeof(testdf)

myarray<-array(1:12,c(2,3,2))
matrix1<-iris[1:2,1:2]
matrix2<-iris[1:2,3:4]
matrix1<-as.data.frame(matrix1)
matrix1<-data.matrix(matrix1)
matrix1%*%matrix1

iris[1,]
plot(iris[,1],iris[,2])
plot(iris$Sepal.Length,iris$Sepal.Width)
plot(iris[,1],iris[,2],main="My first plot")
plot(iris[,1],iris[,2],main="My first plot",xlab="Sepal width")
plot(iris[,1],iris[,2],main="My first plot",xlab="Sepal length",ylab="Sepal width")
plot(iris[,1],iris[,2],main="My first plot",xlab="Sepal length",ylab="Sepal width",col="blue")
plot(iris[,1],iris[,2],main="My first plot",xlab="Sepal length",ylab="Sepal width",col="blue",pch=2)
plot(iris[,1],iris[,2],main="My first plot",xlab="Sepal length",ylab="Sepal width",col="navyblue",pch=3)

hist(iris[,3])
hist(iris[,3],col="pink")
hist(iris[,3],col="pink",main="Histogram of Iris Petal length")
hist(iris[,3],col="pink",main="Histogram of Iris Petal length",freq=FALSE)
hist(iris[,3],col="pink",main="Histogram of Iris Petal length",freq=TRUE)

table(iris$Species)
iristab<-table(iris$Species)
barplot(iristab)
barplot(iristab,main="Iris Species",ylab="frequency",col="gold")

pie(iristab)
pie(iristab,col=c("red","yellow","green"))

pct<-round(iristab/sum(iristab)*100,2)
pct
lbl<-paste(pct,"%")
lbl
pie(iristab,labels=lbl)
lbl<-paste(c("versicolor","setosa","virginica",pct,"%"))

paste("versicolor",30,"%")

boxplot(iris[,1])
boxplot(iris[,1],iris[,2])

head(iris)
var(iris[,1])
sd(iris[,1])
max(iris[,1])
min(iris[,1])
fivenum(iris[,1])

mymean<-function(data){
  x<-sum(data)/length(data)
  return(x)
}
mymean(iris[,1])
mean(iris[,1])

(iris[,1]-10)^2

mysd<-function(data){
  x1<-(data-mean(data))^2
  x2<-sum(x1)/(length(data)-1)
  output<-sqrt(x2)
  return(output)
}
mysd(iris[,1])
sd(iris[,1])