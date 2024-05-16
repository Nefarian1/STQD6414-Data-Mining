##local spatial regression
houses<-read.csv(file.choose(),header = T,sep=";")
head(houses)
#convert to spatial data
library(rspat)
#vect 函数将数据框 houses 转换为 SpatialPointsDataFrame 对象 hvect
hvect<-vect(houses,c("longitude","latitude"))#指定名字为经纬的列
hvect
plot(hvect,cex=0.5,pch=1)#cex = 0.5 控制绘图中点的大小，pch = 1 设置点的形状

###ordinary regression
hd<-houses
#glm 函数拟合一个普通的线性回归模型
#定义了回归方程，其中 houseValue 是因变量（看那个“~”标），其他是自变量
m<-glm(houseValue~income+houseAge+roomhead+bedroomhead+population,data=hd)
summary(m)
#显著性 (Significance):*** 表示非常显著，< 2e-16 表示极其显著
#离散参数 (Dispersion parameter):指定为高斯分布的离散参数
#迭代次数 (Number of Fisher Scoring iterations)
#这个模型中，所有的系数估计都是显著的(看summary的结果)

#local regression绘制县级边界(和上周的差不多)
counties<-spat_data("counties")
crs(hvect)<-crs(counties)
plot(counties)

#从数据框 hd 中选择那些属于特定县（通过 NAME 列匹配）的观测数据
#线性回归模型，其中 houseValue 是因变量
#coefficients(m): 返回线性回归模型的系数估计值
regfun<-function(x){
  dat<-hd[hd$NAME==x,]
  m<-glm(houseValue~income+houseAge+roomhead+
           bedroomhead+population,data=dat)
  coefficients(m)}

#all region names
#新数据框包含所有不是缺失值的
hd2<-hd[!is.na(hd$NAME),]
#提取数据中所有唯一的县名
countrynames<-unique(hd2$NAME)
#每一列对应一个县，每一行对应一个模型系数
res<-sapply(countrynames,regfun)

#convert the data into data frame
resdf<-data.frame(NAME=colnames(res),t(res))

#visualize the results
#aggregate 函数，对县级边界数据的 "NAME" 列进行聚合，以得到唯一的县名
dcounties<-aggregate(counties[,"NAME"],"NAME")
#merge 函数，将县级边界数据和模型系数数据按照 "NAME" 列进行合并
cnres<-merge(dcounties,resdf,by="NAME")
plot(cnres,"income")
cnres2<-cnres
#对 cnres2 中除 "NAME" 列之外的所有列进行标准化（z-score 标准化）
values(cnres2)<-
  as.data.frame(scale(as.data.frame(cnres)[,-1]))
#1:6 表示绘制除 "NAME" 列外的所有列(在前面可以head(resdf)看看)
plot(cnres2,1:6,plg=list(x="topright"),mar=c(1,1,1,1))


###Mining Graph Data
#install.packages("igraph")
library(igraph)

#undirected graph无向图
#例如，1-2 表示连接节点 1 和节点 2 的一条边
g<-graph_from_literal(1-2,1-3,1-7,3-4,2-3,2-4,3-5,4-5,4-6,4-7,5-6,5-8,6-7,7-8)

#input of nodes
#为图的节点添加标签（这里是8个人对应8个节点）
V(g)$name<-c("Adam","Judy","Bobby","Sam","Frank",
             "Jay","Tom","Jerry")
plot(g)

#directed graph有向图，表示三个机场之间的航班方向关系
#three airport:NY,BJ,PR
#有向边通过 -> 和 + 符号来定义，例如，NY-+BJ 表示从纽约（NY）到北京（BJ）的有向边
#那个++就是双向，-+单向，可以搞三角恋了
dg<-graph_from_literal(NY-+BJ,BJ-+PR,PR++NY)
plot(dg)

#weighted graph带权重的无向图
#创建一个数据框，其中包含了一个权重矩阵
m<-read.table(row.names = 1,header = T,text = 
                "A B C D E
              A  0 2 0 5 1.3
              B  1 8 1 0 -4
              C  0 0 0 0 3
              D  5 2 1 0 0
              E  1 1 1 0 3")
m<-as.matrix(m)
#graph.adjacency 函数创建一个带权重的无向图 ig
#参数 mode = "undirected" 表示创建的图是无向图
#weighted = TRUE 表示图中的边带有权重
ig<-graph.adjacency(m,mode = "undirected",weighted = T)
plot(ig,edge.label=E(ig)$weight)

#tree graph
#参数 40 指定图中的节点数量，children = 4 指定每个节点最多有 4 个孩子节点
#mode = "undirected" 表示图是无向的
tr<-make_tree(40,children=4,mode="undirected")
plot(tr,vertex.size=10)

#Bipartite graph二分图
set.seed(23)#设置随机数种子
#10: 第一部分的节点数量，5: 第二部分的节点数量，p = 0.4: 边存在的概率
gb<-sample_bipartite(10,5,p=0.4)
plot(gb)

#define some color & shape为二分图的节点定义了颜色和形状
col<-c("blue","red")
shape<-c("circle","square")
#vertex.color 和 vertex.shape 参数指定节点的颜色和形状
#V(gb)$type 表示每个节点所属的部分
#as.numeric(V(gb)$type) + 1 将部分标识转换为颜色和形状的索引
plot(gb,vertex.color=col[as.numeric(V(gb)$type)+1],
     vertex.shape=shape[as.numeric(V(gb)$type)+1])

#hypergraph超图
#install.packages("HyperG")
library(HyperG)
#函数的参数是一个包含多个集合的列表，每个集合表示超图中的一个超边
h<-hypergraph_from_edgelist(list(1:2,2:5,3:7,
                                 c(1,3,5,7,9)))
plot(h)


#representation of graph
#get.adjlist 函数获取了不同类型图的邻接列表
#Adjacency lists
Adj.list<-get.adjlist(g)#无向图
Adj.list2<-get.adjlist(dg)#有向图
Adj.list3<-get.adjlist(ig)#带权重的无向图
Adj.list4<-get.adjlist(tr)#树状图
Adj.list5<-get.adjlist(gb)#二分图

#Edge list
#get.edgelist 函数获取了不同类型图的边列表
Ed.list<-as.data.frame(get.edgelist(g))
Ed.list2<-as.data.frame(get.edgelist(dg))
Ed.list3<-as.data.frame(get.edgelist(ig))
Ed.list4<-as.data.frame(get.edgelist(tr))

#Adjacency mmatrix获取邻接矩阵
Adj.M<-get.adjacency(g)
Adj.M2<-get.adjacency(dg)
Adj.M3<-get.adjacency(ig)

#Graph manipulation
plot(g)
#从图 g 中移除节点 "Adam" 和 "Judy"，创建了新的图
h1<-g-vertices(c("Adam","Judy"))
#windows(10,10)可加可不加
plot(h1)

#generate subgraph by manual input
#通过 graph_from_literal 函数手动输入了子图的边，创建了一个子图 h2
h2<-graph_from_literal("Adam"-"Farah","Adam"-"Lee",
                       "Adam"-"Bobby","Adam"-"Sam")
plot(h2)

#join graph
#union 函数将两个图 h1 和 h2 进行了合并
h3<-union(h1,h2)
plot(h3)

#modify nodes data
#对图 h3 的节点进行了属性修改
V(h3)#返回图 h3 中的节点列表
#用向量指定了每个节点的性别
V(h3)$gender<-c("male","male","male","female",
                "male","male","male","female",
                "male")
vertex_attr(h3)#显示图 h3 中节点的属性信息

#modify edges data
#对图 h3 的边进行了属性修改
E(h3)$type<-c("email","phone","facebook","twitter",
              "facebook","email","class","phone",
              "twitter","email","facebook","twitter",
              "facebook")#用向量指定了每条边的类型
E(h3)$weight<-c(10,1,3,4,12,4,4,7,5,10,11,3,2)#设置新的权重属性
edge_attr(h3)#显示图 h3 中边的属性信息
plot(h3,edge.label=E(h3)$weight)

#node prominence analysis
#install.packages("statnet")
#install_github("DougLuke/UserNetR")
library(statnet)
library(devtools)
library(UserNetR)

data(Bali)
plot(Bali)

#information about nodes
#获取节点属性中名为 "vertex.names" 的信息
Bali%v%"vertex.names"
#获取节点属性中名为 "role" 的信息
Bali%v%"role"

#information about edges
Bali%e%"IC"

#degree
node<-Bali%v%"vertex.names"
#计算 Bali 数据集中每个节点的度
deg<-degree(Bali)
#数据框 npa，其中包含节点的名称和对应的度
npa<-data.frame(node,deg)

#closeness measure
#closeness 函数计算 Bali 数据集中每个节点的紧密度
cls<-closeness(Bali)
#将之前创建的数据框 npa 与紧密度 cls 合并
npa2<-data.frame(npa,cls)
npa2

#betweeness centrality
#计算 Bali 数据集中每个节点的介数中心性
btw<-betweenness(Bali)
npa3<-data.frame(npa2,btw)

#eigen vector centrality
#计算了 Bali 数据集中节点的特征向量中心性（eigenvector centrality）
evc<-evcent(Bali)

#information centrality score
#计算了 Bali 数据集中节点的信息中心性分数
inf<-infocent(Bali)
npa4<-data.frame(npa3,evc,inf)

#cutpoints
#计算了 Bali 数据集中的割点
net<-Bali
#mode = "graph" 表示计算整个图的割点
#return.indicator = TRUE 表示返回一个逻辑向量，指示每个节点是否为割点
#可以通过打印 cpnet 或直接查看其内容来查看每个节点是否为割点
cpnet<-cutpoints(net,mode="graph",return.indicator = T)


#subgroup analysis
#clique
#该图是一个包含多个团（clique）的例子
library(igraph)
#团是一个图中的完全子图，即其中的每一对节点都相互连接
clqexmp<-graph.formula(A:B:C:D--A:B:C:D,D-E,E-F-G-E)
plot(clqexmp)
#cliques 函数从图 clqexmp 中找到包含至少3个节点的所有团
cliques(clqexmp,min=3)

#large number of clique极大团是在图中不能再添加任何一个节点而保持团的集合
maximal.cliques(clqexmp,min=3)#min = 3 表示只返回包含至少3个节点的极大团

#Bali dataset
#install.packages("intergraph")
library(intergraph)
#将 Bali 数据集转换为 igraph 对象 Bali2
Bali2<-asIgraph(Bali)
#找到包含至少8个节点的所有团
cliques(Bali2,min = 8)

#large number of clique
#maximal.cliques 函数从图 Bali2 中找到所有的极大团，每个团至少包含8个节点
maximal.cliques(Bali2,min=8)

#k-core
#转换 DHHS 数据集为 igraph 对象
data(DHHS)
iDHHS<-asIgraph(DHHS)
plot(iDHHS)
#计算每个节点的核心度
core<-graph.coreness(iDHHS)
table(core)

#modularity
data(Facebook)
plot(Facebook)
#通过 V(Facebook)$group 获取了网络中节点的分组信息
#使用 levels(factor(V(Facebook)$group)) 查看分组的水平
levels(factor(V(Facebook)$group))
#转换分组信息为数值型
grp_num<-as.numeric(factor(V(Facebook)$group))
#计算网络的模块性
modularity(Facebook,grp_num)

#community detection
data(Moreno)
plot(Moreno)
#asIgraph 函数将 Moreno 数据集转换为 igraph 对象 
iMoreno<-asIgraph(Moreno)
#cluster_walktrap 函数进行社团检测，并绘制检测到的社团结构
cw<-cluster_walktrap(iMoreno)
plot(cw,iMoreno)
