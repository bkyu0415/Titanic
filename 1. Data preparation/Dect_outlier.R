#Titanic dataset
#Outlier

library(pastecs)
source("mdist.r")
source("scale.r")
set.seed(12345)

d<- read.csv("~/fill_na.csv")
summary(d)
d = na.omit(d)
#Outlier detection 1
d0 <- d[d$Survived==0,]
d1 <- d[d$Survived==1,]
md0 <- mdist(d0[,c(4,7)])
md1 <- mdist(d1[,c(4,7)])
plot(md0)
plot(md1)
c<- qchisq(0.99,df=2)
x0<- d0[md0<c,]
x1<- d1[md1<c,]
x<- rbind(x0,x1)

#Scaling
write.csv(x,file = "fill_naNout.csv",row.names = F)
