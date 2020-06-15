#Titanic dataset
#ANN
library(nnet)
library(pastecs)
source("ann.r")
source("mdist.r")
source("scale.r")
set.seed(12345)

d<- read.csv("fill_na.csv")
d = na.omit(d)
d$Survived <- as.factor(d$Survived)
summary(d)
#recode character vector & scaling
codechr<-function(v) {
  n<-length(v) # length of v
  h<-levels(v) # store levels in v to h
  k<-length(h) # no. of categories in v
  p<-outer(v,h,"==") # nxk matrix of logical values
  q<-matrix(1:k,nrow=n,ncol=k,byrow=T) # nxk matrix, each row is 1:k
  apply(p*q,1,sum) # output numeric code
}
Sex = codechr(d[,3])
Embarked = codechr(d[,8])
Title = codechr(d[,9])
#Age = scale.con(d[,4])
#Fare = scale.con(d[,7])
d = cbind(d[,c(1,2)],Sex,d[,c(4,5,6,7)],Embarked,Title)

#d = cbind(d[,c(1,2)],Sex,Age,d[,c(5,6)],Fare,Embarked,Title)

# K-fold
n<- nrow(d)
k<-10
set.seed(12345)
idx<-sample(1:n,size=n)
r<-n%%k
if (r==0) {
  mid<-matrix(idx,ncol=k)
} else
  mid<-matrix(idx,ncol=(k+1)) 

nr<-nrow(mid)
err<-NULL

for (i in 1:k) {
  id<-mid[,i]
  d1<-d[-id,] # leave out id as training dataset
  d2<-d[id,]
  
  titanictrain.nn <-ann(d1[,2:9],d1[,1],linout = F ,size = 3, maxit = 200, try = 20)
  pr2 = predict(titanictrain.nn,d2)
  t = table(round(pr2),d2[,1])
  t2 = table(round(predict(titanictrain.nn,d1)),d1[,1])
  err<-c(err,1-sum(diag(t))/sum(t)) # kfCV error
}
t
t2

summary(titanictrain.nn)