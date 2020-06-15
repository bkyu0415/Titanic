#Titanic dataset
#KNN
library(pastecs)
source("mdist.r")
source("scale.r")
source("k_nn.r")
set.seed(12345)

d<- read.csv("~/fill_na.csv")
d$Survived <- as.factor(d$Survived)
summary(d)
d = na.omit(d)
# 
(n<-nrow(d))			# get and display sample size
r<-0.9				# set sampling ratio
id<-sample(1:n,round(r*n))	# create random sample
d1<-d[id,]			# select training data
d2<-d[-id,]	


# knn
cl <- factor(d1[,1])
z1 <- scale.con(d1[,-c(1,2,3,8,9)])
z2 <- scale.con(d2[,-c(1,2,3,8,9)])

w1 <- scale.dum(d1[,c(2,3,8,9)]) 
w2 <- scale.dum(d2[,c(2,3,8,9)]) 

z1 <- cbind(z1,w1)
z2 <- cbind(z2,w2)

#Testing
titanic_knn<-k_nn(z1,z2,cl,d2[,1],v=20)
table(titanic_knn,d2[,1])
