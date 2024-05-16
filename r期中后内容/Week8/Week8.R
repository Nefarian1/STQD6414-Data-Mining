###Mining Time Series Data
set.seed(123)
data1<-rnorm(12)

#create time series data as quarterly data starting
#from 2nd quarter of 2022也就是按季度排，从22年第二季开始
X<-ts(data1, frequency = 4,start=c(2022,2))

#create monthly time series data starting
#from February 2021也就是按月份排，21年2月开始
X2<-ts(data1,frequency = 12, start=c(2021,2))
#检查变量类别用class
class(data1)
class(X)
X2

#r adopting ISO 8601 format
#系统的时间，也就是现在电脑显示的日期和时间
date<-Sys.Date()
time<-Sys.time()

date2<-as.Date("2023-5-12")
date2
class(date2)

#create date starting from 2016-01-01 on to 2018-12-31
daily_index<-seq.Date(from=as.Date("2016-01-01"),
                      to=as.Date("2018-12-31"),
                      by="day")

#3-day interval三天一个数据
day3_index<-seq.Date(from=as.Date("2016-01-01"),
                      to=as.Date("2018-12-31"),
                      by="3 day")

#data dates.formats2.csv
data_df<-read.csv(file.choose(),stringsAsFactors = F,
                  sep=";")
head(data_df)
class(data_df)
str(data_df)

#transform character to date format转化日期格式
attach(data_df)
US_format_New<-as.Date(US_format,format="%m/%d/%Y")
US_format_New
Japanese_format_new<-as.Date(Japanese_format,format="%d/%m/%Y")
Japanese_format_new

#Excel_Numeric_Format
#in excel1, the origin of date is December 30, 1899
#in R, the origin January 1, 1970
#Excel数字日期格式转换为 R 中的日期格式,origin指定了Excel中日期的起点
Excel_f<-as.Date(Excel_Numeric_Format,
                 origin=as.Date("1899-12-30"))
Excel_f
Excel_Numeric_Format

#Class time series data in R

#POSIX class
time_str<-"2017-01-20 23:59:34"
time_str_ctl<-as.list.POSIXct(time_str)

#zoo
install.packages("TSstudio")
library(TSstudio)
library(zoo)
data(US_indicators)
str(US_indicators)
Vehicle_Sales1<-zoo(x=US_indicators$'Vehicle Sales',
                    frequency=12)
Vehicle_Sales1

#ts class(下面那坨是TSstudio包的，可以敲这个ts_plot(USgas)查看)
data(USgas)
class(USgas)

#xts class
library(xts)
version#检查版本用的，有些包可能要更新才能用
Vehicle_Sales2<-xts(x=US_indicators$'Vehicle Sales',
                    frequency=12,
                    order.by=US_indicators$Date)
Vehicle_Sales2

#####Time series decomposition#####
#seasonal component
data(USgas)
ts.plot(USgas, main="US Monthly Natural Gas consumption",
        ylab="Billion Cubic Feet",xlab="year")#时间序列线图
ts_heatmap(USgas)#时间序列热图

#cycle component
data(USUnRate)
ts.plot(USUnRate, main="US Monthly Unemployment Rate",
        ylab="Unemployment Rate",xlab="year")
ts_heatmap(USUnRate)

#Types of decomposition
#additive structure
ts.plot(USgas, main="US Monthly Natural Gas consumption",
        ylab="Billion Cubic Feet",xlab="year")
usg_decompose<-decompose(USgas)#时间序列进行分解，加法结构
plot(usg_decompose)
#extract each component 列出各个组件的名称
names(usg_decompose)
#seasonal component
usg_decompose$seasonal
#trend component
usg_decompose$trend
#random component
usg_decompose$random

#multiplicative structure
data(AirPassengers)
ts.plot(AirPassengers, main="Monthly Airline Passenger",
        ylab="Thousand of Passengers",xlab="year")

air_decompose<-decompose(AirPassengers,
                         type="multiplicative")#分解，乘法结构
plot(air_decompose)

#extract each component
names(air_decompose)
#seasonal component
air_decompose$seasonal
#trend component
air_decompose$trend
#random component
air_decompose$random

#convert multiplicative structure to additive structures.乘法结构变加法
library(forecast)

#multiplicative structure
data(AirPassengers)#名为 AirPassengers 的时间序列数据
#计算 AirPassengers 数据的 Box-Cox 变换的 lambda 值
AirP<-BoxCox.lambda(AirPassengers)
#根据之前计算的 lambda 值进行变换。这个过程旨在将数据转换为对数形式
AirPT<-BoxCox(AirPassengers,lambda = AirP)

par(mfrow=c(1,2))#一行两列，以便同时显示两张图
plot.ts(AirPassengers)
plot.ts(AirPT)

###Time series forecasting
#data Tovel.csv
data<-read.csv(file.choose(),header=T)
yt<-ts(data)
#图片边界par(mfrow=c(2,1))，这个可能大了，反正我换了个显示尺寸喵#
par(mar = c(1, 1, 1, 1))
plot.ts(yt, main="Paper Towel Daily Sales",ylab="yt")

#data is not stationary不平稳
#do a differentation差分操作用于使数据更加平稳
#diff() 函数进行差分操作，减去前一个时间点的值来消除趋势或周期性，使数据更平稳
ztl<-diff(yt,differences=1)
plot.ts(ztl, main="1st difference data",ylab="zt")

#ARIMA model identification
#自相关函数（ACF）和偏自相关函数（PACF），用于帮助识别 ARIMA 模型的参数。
#自相关函数显示了序列与其自身滞后版本之间的相关性，帮助识别时间序列中可能的自相关结构。
acf(ztl, main="Sample Autocorrelation Function")
#偏自相关函数显示了序列在移除其它滞后效应后与当前滞后的相关性，有助于确定时间序列中的部分自相关结构。
pacf(ztl,main="Sample Partial Autocorrelation Function")

#based on ACF & PACF plot
#the model should be ARIMA(0,1,1)

#fit ARIMA(0,1,1) to dataset
#arima() 函数拟合了 ARIMA(0,1,1) 模型到时间序列数据 yt 上
#order = c(0, 1, 1) 指定了模型的阶数，其中 0 表示非季节性自回归（AR）阶数
#"1" 表示非季节性移动平均（MA）阶数
model<-arima(yt,order=c(0,1,1))
summary(model)

#plot model to observed dataset
plot.ts(yt,main="Paper Towel Daily Sales",ylab="yt")
lines(fitted(model),col="red",lty=2)#拟合值

legend("bottomleft",
       c("Observed Data","Model ARIMA(0,1,1)"),
       col=c(1,2),lty=c(1,2))#图例

#Diagnostic check
#residual analysis
library(forecast)
f.value<-forecast(model,h=5)
residual<-f.value$residuals

#residual should not be autocorelated模型的残差（residual）的自相关函数图
acf(residual, lag.max = 20)

#based of ACF plot
#residual is not autocorelated

#residual should approximate normal distribution
#通过观察残差的直方图，可以初步检查残差是否近似于正态分布
hist(residual)
#the residual should having a constant variance over time series
#残差随时间变化的图形。这有助于观察残差是否在时间序列上保持恒定的方差
plot.ts(residual)

#since all the assumptions is fulfiled, the ARIMA(0,1,1)
#model can be used for prediction
#残差看起来在自相关性、正态性和方差稳定性方面都表现得很好，
#这意味着你的 ARIMA(0,1,1) 模型对于预测是合适的。
#这种检查有助于确认模型是否能够准确地捕捉数据的特征，
#并且验证了模型是否可以用于进一步的预测。

#Forecasting from ARIMA(0,1,1)model
plot.ts(yt, main="Paper Towel Daily Sales",ylab="yt")
lines(fitted(model),col="red",lty=2)

forecasting<-predict(model,n.ahead = 5)# 5 步的预测

#50% confidence interval常见的 50% 置信区间
U<-forecasting$pred+0.69*forecasting$se
L<-forecasting$pred-0.69*forecasting$se

#forecasting plot图形中加入了图例，标注了观测数据、预测值和置信区间
ts.plot(yt,forecasting$pred,U,L,col=c(1,2,4,4),lty=c(1,1,2,2))
legend("bottomleft",c("Observed Data","Forecast","Confidence Interval (50%)"),
       col=c(1,2,4),lty=c(1,1,2))

#Time series clustering时间序列聚类
#打开week8中的sample2谢谢喵
#load("D:/RSTUDIO/R project/Week8/sample2.RData")储存位置可改，或File直接开
ts.plot(sample2)

par(mfrow=c(3,3))#绘图区域分割为 3 行 3 列
#plot.ts() 函数来逐一绘制 sample2 中的不同时间序列变量，每个变量占一个小图
plot.ts(sample2[,1]);plot.ts(sample2[,2]);plot.ts(sample2[,3])
plot.ts(sample2[,4]);plot.ts(sample2[,5]);plot.ts(sample2[,6])
plot.ts(sample2[,7]);plot.ts(sample2[,8]);plot.ts(sample2[,9])

#没有就下包喵~#determine the cluster动态时间规整
install.packages("dtw")
library(dtw)

#put a label on each time series data
#创建了标签，用于表示每个时间序列数据点所属的类别
D.Labels<-rep(1:60)

#compute distance matrix
#使用 dist() 函数计算了时间序列数据集 sample2 之间的距离矩阵。
#method = "DTW" 表示使用动态时间规整（DTW）方法计算距离
distMatrix<-dist(sample2,method="DTW")

#plot the cluster
#使用 hclust() 函数进行层次聚类，根据时间序列之间的距离构建聚类结构。
#method = "average" 表示使用平均链接法
TSCluster<-hclust(distMatrix,method = "average")
plot(TSCluster,labels=D.Labels,
     main="Time Series Clustering")

#get 6 cluster将数据分为 6 类的矩形框
rect.hclust(TSCluster,k=6)

#justify your results
#cluster聚类
window(10,10)
par(mfrow=c(2,4))
plot.ts(sample2[,34]);plot.ts(sample2[,35]);plot.ts(sample2[,32])
plot.ts(sample2[,37]);plot.ts(sample2[,33]);plot.ts(sample2[,31])
plot.ts(sample2[,36])

window(10,10)
par(mfrow=c(1,3))
plot.ts(sample2[,18]);plot.ts(sample2[,16])
plot.ts(sample2[,20])

#Time series classification
install.packages("party")
library(party)
#创建了一个类别标签 classId，共有 6 个类别，每个类别包含 10 个样本
classId<-rep(as.character(1:6),each=10)
#打开Data.Ks喵谢谢喵#ctree() 函数进行分类建模，评估了分类模型的预测效果。
Class.Model<-ctree(classId~.,data=Data.Ks)
plot(Class.Model)#绘制了构建的条件推断树模型的可视化结果

#measure precison of prediction model
pClassId<-predict(Class.Model)#得到预测类别
#计算了实际类别和预测类别之间的交叉表，可以帮助评估模型的预测效果
table(classId,pClassId)

#prediction accuracy
#计算了模型的预测准确率，即预测正确的样本数量与总样本数量之比
sum(classId==pClassId)/nrow(Data.Ks)

#这些步骤帮助你构建了一个分类模型，并且评估了该模型对于给定数据的分类准确度。
#通过这些指标，你可以初步了解模型的预测效果和分类准确度。
