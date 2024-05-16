#创建了一个2*3*2的三维数组，第一维有2个元素，第2维有3个元素，
#第3维有2个元素，用数字1到12填充
myarray<-array(1:12,c(2,3,2)) 

#创建了一个2行3列的矩阵，用1到6的数字填充
amfrix<-matrix<-matrix(1:6,nrow=2,ncol=3)

#创建了一个向量，包含4个元素：1，2，3，4
avec<-c(1,2,3,4)
#创建了一个向量，包含3个元素:hello,good,day
vec<-c("hello","good","day")
#创建一个向量，包含1到10的整数数列
vec<-seq(1:10)

#确定内部存储结构或者类型
typeof(vec)

#提取第五个元素
vec[5]
#替换第五个元素为10或字符“test”
vec[5]<-10
vec[5]<-"test"

#将向量放入矩阵中
testm<-matrix(vec,nrow=2,ncol=5)
test<-matrix(1:6,nrow=2,ncol=3)
#将对象转化为数据框
testdf<-as.data.frame(test)


matrix1<-iris[1:2,1:2]#选择数据集的前两行和前两列
matrix2<-iris[1:2,3:4]#选择数据集的前两行和3到4列
matrix1<-as.data.frame(matrix1)#转数据框
matrix1<-data.matrix(matrix1)#转换为数值型矩阵
matrix1%*%matrix1#进行矩阵乘法

iris[1,]#提取第一行元素
plot(iris[,1],iris[,2])#做x=iris第一列元素，y=iris第二列元素的散点图
plot(iris$Sepal.Length,iris$Sepal.Width)#用列名做散点图
plot(iris[,1],iris[,2],main="My first plot",xlab="Sepal length",ylab="Sepal width")#加x轴，y轴标签和标题
plot(iris[,1],iris[,2],main="My first plot",xlab="Sepal length",
     ylab="Sepal width",col="navyblue",pch=3)#给散点加颜色，改变散点的形状

hist(iris[,3])#用第3列做直方图
hist(iris[,3],col="pink",main="Histogram of Iris Petal length",freq=FALSE)#颜色，加标题,y轴显示他的密度
hist(iris[,3],col="pink",main="Histogram of Iris Petal length",freq=TRUE)#y轴显示频率

table(iris$Species)#显示每一列里有什么类目
iristab<-table(iris$Species)
barplot(iristab,main="Iris Species",ylab="frequency",col="gold")#柱状图
pie(iristab,col=c("red","yellow","green"))#饼图，填色

pct<-round(iristab/sum(iristab)*100,2)#计算每个物种所占的百分比，并保留两位小数，round()保留几位小数
lbl<-paste(pct,"%")#加百分比符号
lbl<-paste(c("versicolor","setosa","virginica",pct,"%"))#添加变量名称
pie(iristab,labels=lbl)#在饼图上显示变量名

boxplot(iris[,1])#用一列数据画一个箱型图
boxplot(iris[,1],iris[,2])#用两列数据画出箱型图

var(iris[,1])#计算数据某一列的样本方差
sd(iris[,1])#计算某一列数据的样本标准差
max(iris[,1])#最大值
min(iris[,1])#最小值
fivenum(iris[,1])#算一组数字的最小值、下四分位数、中位数、上四分位数和最大值。

#两种计算数列均值的办法
mymean<-function(data){
  x<-sum(data)/length(data)
  return(x)
}
mymean(iris[,1])
mean(iris[,1])

#计算数列标准差的方法
mysd<-function(data){
  x1<-(data-mean(data))^2
  x2<-sum(x1)/(length(data)-1)
  output<-sqrt(x2)
  return(output)
}
mysd(iris[,1])
sd(iris[,1])
