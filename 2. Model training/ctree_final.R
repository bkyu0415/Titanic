#Titanic dataset
#C-tree
library(pastecs)
source("mdist.r")
source("scale.r")
source("LC.r")
set.seed(12345)

d<- read.csv("~/fill_na.csv")
summary(d)
d = na.omit(d)
# 
(n<-nrow(d))			# get and display sample size
r<-0.9				# set sampling ratio
id<-sample(1:n,round(r*n))	# create random samplea
d1<-d[id,]			# select training data
d2<-d[-id,]	
# ctree d1 & Lchart
ctree<-rpart(Survived~.,data = d1,method="class")
pr<-predict(ctree)				
c<-max.col(pr)
table(c,d1$Survived)

ps = pr[,2]
LChart(d1$Survived,ps,col='blue')



#random forest
n1<-nrow(d1)
n2<-nrow(d2)
ns<-100
pred<-matrix(0,nrow=n2,ncol=ns)
for (i in 1:ns) {
  id<-sample(1:n1,replace=T)
  ds<-d1[id,]
  ctree<-rpart(ds[,1]~.,data=ds[,2:9],method="class",minsplit=40,maxdepth=4)
  pr<-predict(ctree,d2) 
  cl<-max.col(pr)-1 
  pred[,i]<-cl
}
pr<-apply(pred,1,mean) 
pr1<-(pr>0.5)+0

table(pr1,d2$Survived)
library(rpart.plot)
rpart.plot(ctree,extra=101)
print(ctree)


