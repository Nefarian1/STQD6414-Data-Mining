##Minig Spatial Data##
#空间数据的简单处理
library(sp)
library(raster)

#raster data manipulation
#其中参数ncol和nrow指定了栅格的列数和行数，
#而xmx、xmn、ymn和ymx则定义了栅格的空间范围，分别表示最大和最小的经度和纬度
r<-raster(ncol=10,nrow=10,xmx=-80,xmn=-150,
          ymn=20,ymx=60)
income<-rnorm(ncell(r))#生成一个与栅格单元数相同的随机数向量
values(r)<-income#将之前生成的收入数据向量income赋值给raster对象r的值
plot(r)

#algebra in raster data
r2<-r+10
r3<-r^3
r4<-2*r2+r3
s<-stack(r,r2,r3,r4)#四个图层的栅格数据的堆叠
plot(s)

#extract raster layer from raster stack/brick
rL3<-raster(s,layer=3)#提取第三个图层（layer=3），并将其存储在 rL3 中
plot(rL3)

#add new value in the raster data
#生成一个随机数向量 debt，其中包含了长度与 r 栅格对象的栅格单元数相同的数值
#这些数值是通过 rgamma 函数生成的伽马分布的随机数
#并且每个数值的相反数被计算（(-1) * rgamma(...)）
debt<-(-1)*rgamma(length(r),20,30)
r$debt<-debt#将新图层添加
plot(r)#添加前以及添加后的两图

#crop raster data裁剪栅格数据，即限制数据集的范围以仅包含感兴趣的部分
#横坐标（经度）范围在 -150 到 -120，纵坐标（纬度）范围在 20 到 40 之间的部分
r1<-crop(r,extent(-150,-120,20,40))
plot(r1)
r2<-crop(r,extent(-120,-110,40,60))#这个和上面同理
plot(r2)

#merge raster data
#将两个裁剪后的栅格数据对象 r1 和 r2 合并成一个新的栅格数据对象 m
m<-merge(r1,r2)
plot(m)

#Descriptive statistics
#cellStats 函数用于计算栅格数据的统计量
#第一个参数是栅格对象
#第二个参数是要计算的统计量的函数
#mean均值
cellStats(r,mean)
#standard deviation标准差
cellStats(r,sd)
#median中位数
cellStats(r,median)

#Spatial Autocorrelation空间自相关
#shapefile 函数读取一个 Luxemburg 行政区划的形状文件（shapefile）
p<-shapefile(system.file("external/lux.shp",
                         package = "raster"))
plot(p)
data.frame(p)#对象 p 转化为数据框，以便查看其中的数据

#创建一个新的对象 p1，该对象包含原始数据中行政区划名称为 "Diekirch" 的部分
p1<-p[p$NAME_1=="Diekirch",]
plot(p1)
p1$value<-c(10,6,4,11,6)#向选定的 "Diekirch" 区域添加一个名为 "value" 的新变量

#没有就install.packages("spdep")
library(spdep)
#poly2nb 函数创建一个空间邻接矩阵（spatial weights matrix）
#这个邻接矩阵描述了地理空间上的相邻关系
w<-poly2nb(p1,row.names = p1$ID)
#nb2listw 函数将邻接矩阵转换为 "listw" 对象
#这是进行空间统计分析时常用的一种表示方式
ww<-nb2listw(w)
#对选定区域 "Diekirch" 中的 "value" 变量执行莫兰指数（Moran's I）检验
#ww 是之前创建的空间权重矩阵，randomisation = FALSE 表示不进行随机化检验
moran.test(p1$value,ww,randomisation = F)
#莫兰指数是一种用于测量地理空间中数据的空间相关性的统计指标
#检验的假设是数据是否在空间上呈现显著的自相关性
#如果 p 值小于显著性水平，就可以拒绝空间自相关性的零假设，表示存在空间自相关性。
#由于 p 值小于显著性水平（0.008524 < 0.05），我们可以拒绝零假设
#得出结论：在 "Diekirch" 区域中的 "value" 变量具有正的空间自相关性
#表明在该地区，相邻地理单元之间的 "value" 值更可能相互关联，而不是随机分布

#Spatial Interpolation空间插值
#install.packages("devtools")
library(devtools)
#install_github("rspatial/rspat")
library(rspat)
#spat_data 函数加载一个名为 'precipitation' 的空间数据集
#这个数据集可能包含有关降水量的信息，可以在空间上进行插值
d<-spat_data('precipitation')

#plot a data
#指定经度（LONG）和纬度（LAT）作为空间坐标
#设置坐标参考系（CRS）。在这里，CRS 被设置为 WGS84
dsp<-vect(d,c("LONG","LAT"),
          crs="+proj=longlat+datum=WGS84")
#加载名为 "counties" 的示例空间数据集
#plot(CA) 用于绘制美国的县级边界
CA<-spat_data("counties")
plot(CA)

#determine the cumulative precipitation
#这段代码用于计算累积降水量，并将结果添加到空间数据对象 d 中
mnts<-toupper(month.abb)#创建一个包含英文缩写月份的字符向量，将其转换为大写形式
d$prec<-rowSums(d[,mnts])#创建了一个新的列，包含了每个点的累积降水量（月）
#老样子设置坐标参考系（CRS）
dsp<-vect(d,c("LONG","LAT"),
          crs="+proj=longlaat+datum=WGS84")

#define some threshold
#数值将被用作累积降水量的分割点，将其分成不同的类别
#一个路过的吸血鬼说：“你刚刚用了cuts了，对吧？！”
cuts<-c(0,200,300,500,1000,3000)

#plot the data based on threshold
#颜色渐变将用于表示不同累积降水量类别
blues<-colorRampPalette(c('yellow','orange','blue','dark blue'))
#函数绘制美国县级边界，border='dark gray' 设置边界的颜色为深灰色
plot(CA,border='dark gray')
#参数 "prec" 表示使用 dsp 对象中的 "prec" 列
#breaks 参数指定了阈值，将累积降水量分成不同的类别：“用了cuts，对吧？！”
plot(dsp,"prec",breaks=cuts, col=blues(10))

#Spatial interpolation
#proximity polygons
windows(10,10)#10x10 的绘图窗口
#生成 dsp 中点的 Voronoi 多边形
#Voronoi 多边形是由空间中离散点到最近邻的距离确定的多边形。
v<-voronoi(dsp)
plot(v)
#points 函数将原始的 dsp 数据点绘制到 Voronoi 图形上
points(dsp)
#crop 函数将 Voronoi 多边形限制在美国的县级边界范围内
vca<-crop(v,CA)
#绘制裁剪后的 Voronoi 多边形，其中颜色表示预测的累积降水量
#我对吸血鬼说：“喂，你刚才又用了prec是吧？”
plot(vca,"prec")
