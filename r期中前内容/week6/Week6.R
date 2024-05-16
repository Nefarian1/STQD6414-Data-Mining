###Set1_data_fa.csv
data<-read.csv("Set1_data_fa.csv",header = TRUE)
attach(data)
  
###
meancol<-apply(data,2,mean)
meancolmat<-t(replicate(dim(data)[1],meancol))
data_c<-data-meancolmat
covX<-cov(data_c)
summary(data_c)
eig<-eigen(covX)

explained_var<-eig$values
explained_var
pevar<-explained_var/sum(explained_var)*100
pevar
cumsum(pevar)
plot(1:11,pevar,type="l")

V<-eig$vectors
pc<-as.matrix(data_c)%*%V
reduceddata<-pc[,1:4]
head(reduceddata)

###factor analysis
install.packages("psych")
data<-read.csv("Set1_data_fa.csv",header = TRUE)
out<-psych::fa(data,nfactors=4,rotate="varimax")
out$loading
out$scores
reduceddata<-out$scores

lambda<-out$loadings
psi<-diag(out$uniquenesses)
estcov<-lambda%*%t(lambda)+psi
oricov<-cov(scale(data))
round(oricov-estcov,5)

###apriori algorithm
data<-read.csv("Set1_retail.csv",header = TRUE)
datacomplete<-data[complete.cases(data),]
head(data)
dim(data)
dim(datacomplete)
require(tidyverse)
datacomplete%>%group_by(invoiceno)%>%summarize(n_total=n(),
                                               n_items=n_distinct(stockcode))

install.packages("arules")
require(arules)

split(datacomplete$description,datacomplete$invoiceno)
head(datacomplete)
datacompletelist<-split(datacomplete$description,datacomplete$invoiceno)
trx<-transactions(datacompletelist)
ap<-apriori(trx,parameter = list(supp=0.05,conf=0.8))
inspect(head(ap,10))

###set transaction names
a_list<-list(c("a","b","c"),
             c("a","b"),
             c("a","b","d"),
             c("c","e"),
             c("a","b","d","e"))

names(a_list)<-paste("Tr",c(1:5),sep="")
a_list
