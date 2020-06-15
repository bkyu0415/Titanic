#Titanic dataset
#Missing values processing

library(pastecs)
source("mdist.r")
source("scale.r")
set.seed(12345)

d<- read.csv("~/Titanic_whole_dataset.csv")
summary(d)


# Split dataset into missing data and non-missing data
d_miss = d[is.na(d$Age),]
d_miss = d_miss[,-c(4)]
d_notmiss = d[!is.na(d$Age),]
d = d[!is.na(d$Age),]

#Fill NA for column Age by Model

library(nnet)
titanic_age.nn <- nnet(Age~.,data = d, size = 5, linout = T)
Age = predict(titanic_age.nn,d_miss)
data = cbind(d_miss,Age)
dc = rbind(d_notmiss,data)

#write to CSV
write.csv(dc,file = "fill_na.csv",row.names = F)
