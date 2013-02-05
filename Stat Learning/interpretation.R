set.seed(1219)
real<-read.csv("real.csv")
real.l<-real
real.l[, 15]<-log(-real[,15])

library(rpart)
library(gbm)
library(randomForest)

glm.orig <- glm(REMSN~MARITB+EMPLB+SCHOOL+MKEDC+ENJOY+FAMIM+SEX+HSPNC+WHITE+BLACK+age+dage+EPLT5+episode_date+ATMTSUIC+L1SE+L1EFCCY+MDSWCH1+MDAUG1+ssri+L2BEG+L1BEG+L1slope, family = binomial, data=real)
summary(glm.orig)

#glm.ov <- glm(REMSN~MARITB+SCHOOL+SEX+ATMTSUIC+L1EFCCY+MDSWCH1+L2BEG+L1BEG+L1slope, family = binomial, data=real)
#summary(glm.ov)

glm.inter <- glm(REMSN~MARITB+EMPLB+SCHOOL+MKEDC+ENJOY+FAMIM+SEX+HSPNC+WHITE+BLACK+age+dage+EPLT5+episode_date+ATMTSUIC+L1SE+L1EFCCY+MDSWCH1+MDAUG1+ssri+L2BEG+L1BEG+L1slope+ssri*(L1SE+L1EFCCY+L1BEG+L2BEG+L1slope+MDSWCH1+MDAUG1), family = binomial, data=real.l)
summary(glm.inter)

glm.iv <- glm(REMSN~MARITB+SEX+ATMTSUIC+L1EFCCY+MDSWCH1+ssri+L2BEG+L1BEG+L1slope+ssri*(MDSWCH1), family = binomial, data=real.l)
summary(glm.iv)


single.tree<-rpart(REMSN~.-ID, data=real, method = "class")
single.tree
single.tree2<-rpart(REMSN~.-ID, data=real, method = "class", minsplit = 40, cp=0.005)
single.tree2

X<-model.matrix(~.-ID-REMSN+ssri*(L1SE+L1EFCCY+L1BEG+L2BEG+L1slope+MDSWCH1+MDAUG1),data=real.l) 
rf.n <- randomForest(X,y=as.factor(real$REMSN), keep.forest = T)
w<- rf.n$importance

dump(c("w", "rf.save"), "rf.R")
#source('rf.R')
#rf.save$importance

gbm1<-gbm(REMSN~.-ID+ssri*(L1SE+L1EFCCY+L1BEG+L2BEG+L1slope+MDSWCH1+MDAUG1), data=real.l, distribution = "bernoulli", shrinkage = 0.1, n.trees = 400)
summary(gbm1)


