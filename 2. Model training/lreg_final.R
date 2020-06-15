#Titanic dataset
#Logistic regression

library(pastecs)
source("mdist.r")
source("scale.r")
set.seed(12345)

d<- read.csv("~/fill_naNout.csv")

summary(d)
d = na.omit(d)

n<- nrow(d)
# k-fold CV
k<-10
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
  x1<-d[-id,] # leave out id as training dataset
  x2<-d[id,]
  lreg <- glm(Survived~.,data = x1,binomial)
  #Testing
  pr<-predict(lreg,x2) # compute x'b
  prob<-exp(pr)/(1+exp(pr))
  pred<-(prob>0.5)+0 # save pred
  t<-table(pred,x2$Survived)
  err<-c(err,1-sum(diag(t))/sum(t)) # kfCV error
  #traning
  pr3<-predict(lreg,x1) # compute x'b
  prob3<-exp(pr3)/(1+exp(pr3))
  pred3<-(prob3>0.5)+0 # save pred
  t3<-table(pred3,x1$Survived)
}

t
t3
summary(lreg)
