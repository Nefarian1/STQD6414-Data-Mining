#Mining Text Data#
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("syuzhet")
install.packages("ggplot2")

library(tm)#文本挖掘
library(SnowballC)
library(wordcloud)#词云
library(RColorBrewer)
library(syuzhet)
library(ggplot2)

#import the data选择text文件
text<-readLines(file.choose())
class(text)

#define a corpus text创建了一个文本语料库（corpus）
docs<-Corpus(VectorSource(text))
class(docs)
inspect(docs)#查看创建的语料库的内容

#Data cleaning of text data

#(i)remove special character from text
#define blank space
#替换为空格
toSpace<-content_transformer(function(x,pattern)
  gsub(pattern,"",x))

#convert all special character into blank space

docs2<-tm_map(docs,toSpace,"/")
docs2<-tm_map(docs2,toSpace,"@")
docs2<-tm_map(docs2,toSpace,"!")
docs2<-tm_map(docs2,toSpace,",")
docs2<-tm_map(docs2,toSpace,":")

#(ii)Convert the text to lower case小写
docs2<-tm_map(docs2,content_transformer(tolower))

#(iii)remove numbers移除文本中的数字
docs2<-tm_map(docs2,removeNumbers)

#(iv)remove the stopwords移除英文停用词
docs2<-tm_map(docs2,removeWords,
              stopwords("english"))

#(v)remove punctuation移除标点符号
docs2<-tm_map(docs2,removePunctuation)

#(vi)eliminate extra unnecessary spaces in the text
#消除文本中额外的空格
docs2<-tm_map(docs2,stripWhitespace)

###Word tokenization
library(tidyverse)
library(tokenizers)
#tokenize_words函数对文本进行了词语分词化，
#将文本拆分成单词列表，保存在words.Tk变量中
words.Tk<-tokenize_words(text)

#Text stemming提取词干（stem），即将单词转换为其词根形式，存储在docs3变量
docs3<-tm_map(docs2,stemDocument)
inspect(docs3)#展示了处理后的文本数据

#Document-term matrix
#TermDocumentMatrix函数将经过处理的文本数据集docs3转换为文档-词项矩阵（DTM），
#将单词的出现频率映射到一个矩阵中，保存在dtm变量中
dtm<-TermDocumentMatrix(docs3)
m<-as.matrix(dtm)
m

#the frequency table of words词频表
#rowSums(m)用于计算每个单词在整个文本数据集中的总出现次数
#sort(..., decreasing = TRUE)对单词按照频率进行排序
v<-sort(rowSums(m),decreasing = T)
#word列存储单词，freq列存储对应的频率
d<-data.frame(word=names(v),freq=v)

#plot word frequency
#first 20 most frequent words最高的前20个单词的频率
#las=2参数将标签文本旋转90度
#names.arg=d[1:20,]$word设置了条形图中每个条形的标签，前20个高频单词内容
barplot(d[1:20,]$freq,las=2, names.arg=d[1:20,]$word,
        main="Most frequent words")#main="Most frequent words"图表标题

###################################################
#word cloud
set.seed(12)
#words=d$word是用于词云的单词列表，freq=d$freq包含了每个单词的频率信息
#min.freq=2设置词云中单词最小出现频率，max.words=100定义了词云中最大单词量
#random.color=F禁用了随机颜色
wordcloud(words=d$word, freq=d$freq, min.freq=2,
          max.words = 100,random.color=F,
          color=brewer.pal(8,"Dark2"))

#Word association（findAssocs函数用于分析词语之间的关联性）
#analyze the association between frequent words
#Example:which words are associated "freedom"?
#freedom”相关联的单词，其相关性阈值为0.3
findAssocs(dtm,terms="freedom",corlimit=0.3)$freedom

#Example:association between some particular words
#展示三个单词之间的相关性
findAssocs(dtm,terms=c("freedom","dream","will"),corlimit=0.25)

#Example:find association for words that occur
#at least 10 times至少出现10次的词
findAssocs(dtm,terms=findFreqTerms(dtm,lowfreq = 10),corlimit = 0.25)


##Sentiment Analysis
library(sentimentr)
x<-'Sentiment analysis is super fun'
sentiment(x)#正面情感
y<-'Sentiment analysis is super boring. I do love work'
sentiment(y)#包含正面和负面情感的复合情感
#get_sentiment函数则用于对整个文本进行情感分析
#method="syuzhet"来采用了"syuzhet"方法进行情感分析
sentiment_sz<-get_sentiment(text,method="syuzhet")

hist(sentiment_sz)
summary(sentiment_sz)

##Emotion classification用了NRC情感词典（NRC Sentiment Lexicon）
#get_nrc_sentiment函数用于基于NRC情感词典对文本进行情感分类
#列代表不同的情感类别，行代表文本中的每个词或短语
Ec<-get_nrc_sentiment(text)

td<-data.frame(t(Ec))#计数
td_new<-data.frame(rowSums(td))#新数据框，每行表示一种情感类别的总计数
td_new

##transformation & cleaning
names(td_new)[1]<-"count"#数据框td_new的第一列更改列名为"count"
#将数据框td_new的行名（情感类别）添加为一列，并命名为"sentiment"
td_new<-cbind("sentiment"=rownames(td_new),td_new)
rownames(td_new)<-NULL#td_new的行名设为NULL，将其重置
td_new2<-td_new[1:8,]#只显示前8个
#条形图的高度代表了每种情感类别的计数，通过不同的颜色进行区分
quickplot(sentiment, data=td_new2,weight=count,
          geom="bar",fill=sentiment,ylab="count")+
  ggtitle("Survey Sentiments")


#################################################
#Mining Spatial Data
library(sp)
library(raster)
load("D:\\RSTUDIO\\R project\\week11\\wst.RData")
attach(wst)#变量直接放入当前环境

lonlat<-cbind(longitude,latitude)#数据集的经度和纬度组成一个矩阵lonlat
pts<-SpatialPoints(lonlat)#经纬度坐标点
class(pts)
plot(pts)

#define coordinate system
crdref<-CRS('+proj=longlat +datum=WSG84')#定义了坐标系
#坐标系信息应用到 SpatialPoints 对象
pts<-SpatialPoints(lonlat,proj4string = crdref)

#embed the atribut to SpatialPoints vector
df<-data.frame(ID=name,precip)#新数据框包含了名称和降水量
ptsdf<-SpatialPointsDataFrame(pts,data=df)
plot(ptsdf)
showDefault(ptsdf)

#spatial lines data这里spLines()函数用于创建空间线数据
lonlat<-cbind(longitude,latitude)
linesD<-spLines(lonlat,crs=crdref)
plot(linesD)

#spatial polygom data创建空间多边形数据
lonlat<-cbind(longitude,latitude)
polyD<-spPolygons(lonlat,crs=crdref)
plot(polyD)

#raster form data创建栅格形式的空间数据
r<-raster(ncol=10,nrow=10,xmx=-80,xmn=-150,
          ymn=20,ymx=60)
area<-rnorm(ncell(r))#随机数，范围r
values(r)<-area
plot(r)

#raster stack & raster brick
#stack() 函数将这四个栅格对象组合成一个栅格堆叠（stack）。
#栅格堆叠是将多个栅格对象堆叠到一个对象中，每个栅格对象对应于堆叠中的一个层
r2<-r*r
r3<-r^3
r4<-2*r2+r3
s<-stack(r,r2,r3,r4)
plot(s)

#data manipulation in spatial analysis
#下面找到了 raster 包中的 lux.shp 文件的路径，矢量空间数据文件 lux.shp
f<-system.file("external/lux.shp",package = "raster")
p<-shapefile(f)

#presenting data in data  frame format
d<-data.frame(p)

#extract spesific atributes提取空间数据对象 p 中的特定属性
p$NAME_1
p$AREA

#add new variable
set.seed(12)
xp<-sample(letters,length(p))
p$new<-xp#生成的随机字母序列 xp 添加为 p 对象的一个新变量 new
data.frame(p)#创建了一个数据框，显示了经过变量添加和删除后的空间数据对象 p

#delete any variable删除了 p 对象中的 new 变量
p$new<-NULL

#data integration
dfr<-data.frame(District=p$NAME_1,Canton=p$NAME_2,
                Value=runif(length(p)))

cD<-merge(p,dfr,by.x=c('NAME_1','NAME_2'),
          by.y=c('District','Canton'))

#data aggregation绘制地图
pa<-aggregate(p,by='NAME_2')#aggregate() 函数按NAME_2 属性进行聚合
plot(p)
plot(pa,add=T,col=rainbow(12))

#map manipulation
