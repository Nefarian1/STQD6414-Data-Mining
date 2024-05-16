###
set.seed(123)
data<-rnorm(10000,5,3)
data
n<-1000
reserve<-data[1:1000]
#check
data[1000:1005]
reserve[999:1000]

set.seed(123)
runif(1)
1000/1001
runif(1)<=1000/1001
runif(10)<=1000/1001
runif(10)<=0.5

set.seed(123)
table(runif(1000)<=0.9)

set.seed(123)
table(runif(1)<=1000/1001)

###
set.seed(123)
data<-rnorm(10000,5,3)
sample(1:1000,1)#roll a number
reserve<-data[1001]
reserve
data[1002]
reserve[597]<-data[1002]
reserve[597]
table(runif(sample(1:1000,1))<=1000/1001)

###
set.seed(123)
for(i in 1001:10000){
  if(runif(1)<=(1000/i)){
    id<-sample(1:1000,1)
    reserve[id]<-data[i]
  }
}

set.seed(123)
data<-rnorm(10000,5,3)
reservesample<-function(data,n){
  reserve<-data[1:n]
  N<-length(data)
  for(i in (n+1):N){
    if(runif(1)<=(n/i)){
      id<-sample(1:n,1)
      reserve[id]<-data[i]
    }
  }
  return(reserve)
}
reserve

set.seed(123)
reservesample(data,1000)

###
reservesample<-function(data,n){
  reserve<-data[1:n]
  N<-length(data)
  i<-n+1
  while(i<=N){
    if(runif(1)<=(n/i)){
      id<-sample(1:n,1)
      reserve[id]<-data[i]
    }
    i<-i+1
  }
  return(reserve)
}

###reservoir sampling
reservesample<-function(data,n){
  reserve<-data[1:n]
  N<-length(data)
  for(t in (n+1):N){
    u<-sample.int(t,1)
    if(u<=n){
      reserve[u]<-data[t]
      return(reserve)
    }
  }
  return(reserve)
}
reservesample(data,1000)
x<-reservesample(data,1000)
mean(x)
sd(x)







