##mining sequence data
install.packages('TraMineR')
library(TraMineR)

#example of sequence data
data(mvad)
summary(mvad)#摘要

##define a sequence data
#define a labels and codes of codes for state
#例如:"employment"被定义为"EM"
mvad.labels<-c("employment","further education",
               "higher education","joblessness",
               "school","training")
mvad.scode<-c("EM","FE","HE","JL","SC","TR")

#create sequence object in R
#使用了从第17列到第86列的数据来创建序列对象，并使用之前定义的状态代码和标签。
mvad.seq<-seqdef(mvad, 17:86, states=mvad.scode,
                 labels=mvad.labels)

#statistical summary indicators
#mean time spent in each state
#overall dataset每种状态（就业、进修、失业等）的平均持续时间
seqmeant(mvad.seq)

#mean time spent in each state by group
#group:fathers employment status
#by()函数，这次是按照"XX"对数据进行分组，
#并计算了每个分组内每种状态的平均持续时间。
#这可以用来比较不同XX情况的年轻人在每种状态上的平均持续时间。
by(mvad.seq, mvad$funemp, seqmeant)

#group:gender
by(mvad.seq, mvad$male, seqmeant)

#number of transition状态转换的次数
nT<-seqtransn(mvad.seq)
hist(nT)#直方图

#transition rates转换率
#overall dataset
mvad.trate<-seqtrate(mvad.seq)

#if the transition rate different over time
#time varying transition rates
mvad.trate2<-seqtrate(mvad.seq, time.varying = T)

##Visualizing sequence data
#sequence index plot序列索引图
#每一条线代表一个序列，横轴表示序列的时间顺序，
#纵轴表示数据集中的序列编号。这有助于观察序列在数据集中的分布情况
seqiplot(mvad.seq, border=NA,
         main="Sequence index plot")

#sequence frequency plot序列频率图
seqfplot(mvad.seq, border=NA,
         main="Sequence frequency plot")

#sequence distribution plot序列分布图
#观察数据集中序列的多样性和分布情况
seqdplot(mvad.seq, border=NA,
         main="Sequence distribution plot")

#model state plot模态状态图
#展示了每个时间点上最常见的状态
seqmsplot(mvad.seq, border=NA,
         main="Modal state plot")

#transversal entropy横向熵图
#横向熵是一种度量序列模式多样性的指标，
#它表示在给定时间点上不同序列的多样性程度
seqHtplot(mvad.seq, main="Transversal Entropies")

#event sequence of transition
#define event:sequence of transition
mvad.seqe<-seqecreate(mvad.seq)

#look for frequent event subsequences
#seqefsub() 函数通过事件序列 mvad.seqe 找到频繁出现的事件子序列，
#参数 pmin.support = 0.05 表示最小支持度为 0.05，即出现频率至少为 5%
fsubseq<-seqefsub(mvad.seqe, pmin.support = 0.05)

#plot 15 most frequent event前15个最频繁的事件子序列
plot(fsubseq[1:15], col="blue")


##Categorizing pattern in sequence data聚类算法
library(cluster)

#compute optimal matching distance based on transition rate
#这行代码计算了基于转换率的最优匹配距离。
#seqsubm() 函数根据指定的方法（这里是"TRATE"，即基于转换率），
#计算了序列对象 mvad.seq 中每对序列之间的相似性矩阵。
#这个相似性矩阵描述了序列之间的相似程度
submat<-seqsubm(mvad.seq, method="TRATE")

#这行代码使用最优匹配距离（Optimal Matching，OM）来计算序列数据的距离矩阵。
#seqdist() 函数计算了基于最优匹配的序列之间的距离，
#并使用了之前计算的相似性矩阵 submat。这个距离矩阵描述了序列之间的差异程度。
dist.om<-seqdist(mvad.seq, method="OM", sm=submat)

#build hierarchical  clustering  based on
#optimal matching distance
#agnes()函数进行聚类分析。
#agnes()函数执行了AGNES（AGglomerative NESting）层次聚类算法，
#它接受一个距离矩阵 dist.om 作为输入，并使用ward方法来构建聚类。
#diss = TRUE 表示输入的是距离矩阵
clusterward<-agnes(dist.om, diss=T, method="ward")
plot(clusterward)#按<Return>键来看下一个图

#retrieve each sequence data for some cluster
#example:k=4 cluster
#cutree()将层次聚类的结果切割为指定数量的聚类， k = 4，即切割成4个聚类。
cl.4<-cutree(clusterward, k=4)
#每个聚类分配了一个标签
#用了paste()函数来生成标签，例如："type 1"、"type 2"、"type 3"、"type 4"
cl.4fac<-factor(cl.4, labels=paste("type",1:4))

seqiplot(mvad.seq,group=cl.4fac, border=NA,
         main="Sequence index plot")

seqfplot(mvad.seq,group=cl.4fac, border=NA,
         main="Sequence frequency plot")

seqdplot(mvad.seq,group=cl.4fac, border=NA,
         main="Sequence distribution plot")

seqmsplot(mvad.seq, border=NA,group=cl.4fac,
          main="Modal state plot")

seqHtplot(mvad.seq,group=cl.4fac, main="Transversal Entropies")

#Determine the most discriminating transitions
#between cluster and plot the frequencies by cluster
#seqecmpgroup()函数来确定不同聚类之间具有区分性的转换。
#它分析了之前找到的频繁事件子序列 fsubseq 在每个聚类中的出现频率差异，
#从而确定哪些转换最能区分不同的聚类。group = cl.4fac 指定了要分析的聚类变量。
discr<-seqecmpgroup(fsubseq, group=cl.4fac)
plot(discr[1:6])#频率最高的前6个
