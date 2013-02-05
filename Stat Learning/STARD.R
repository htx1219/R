set.seed(1219)
source("datasetl.R")
library(e1071)
library(rpart)
library(gbm)
library(dismo)
library(randomForest)
library(nnet)
library(ada)
# library(lrm)
# library(penalized)
library(glmnet)
# library(lasso2) #gl1ce
source("Model.R")

print.compare<-function(all){
	cat("Among", all$positiveall, "positive cases,", all$realpositive, "are real positive,", all$falsepositive, "are false positive, and the false positive rate is", round(100*all$fprate, digit=2), "%\n")
	cat("Among", all$negativeall, "negative cases,", all$realnegative, "are real negative,", all$falsenegative, "are false negative, and the false negative rate is", round(100*all$fnrate, digit=2), "%\n")
	cat("The classification error rate is", round(100*all$errorrate, digit=2), "%\n")
	try(cat("The Chi-square value for McNemar test is", round(all$mcnestat, digit=2), ", and the p value is", round(all$mcnep, digit=4), "\n"))
}

compare<-function(pred.p, real, ret=F, print = T){
	if (class(pred.p)=='factor'){
		if (levels(pred.p)[1] == "0"){
			pred.p<-as.numeric(pred.p)-1
		}else{
			cat("please do something")
		}
	}
	pred<-(pred.p>0.5)
	real<-(real>0.5)
	pred.t<-(pred==1)
	pred.f<-(pred==0)
	all<-list()
	all$positiveall<-sum(pred.t)
	all$realpositive<-sum(real[pred.t])
	all$falsepositive<-sum(1-real[pred.t])
	all$fprate<-sum(1-real[pred.t])/sum(pred.t)
	all$negativeall<-sum(pred.f)
	all$realnegative<-sum(1-real[pred.f])
	all$falsenegative<-sum(real[pred.f])
	all$fnrate<-sum(real[pred.f])/sum(pred.f)
	all$errorrate<-(1-mean(pred==real))
	# all$mcnestat<-"Not avaliable"
	# all$mcnep<-"Not avaliable"
	try(mcne<-mcnemar.test(pred, real))
	try(all$mcnestat<-mcne$statistic)
	try(all$mcnep<-mcne$p.value)
	if (print){
	print.compare(all)
	}
	if (ret){
	return(all)
	}
	}

# The Goal
p = mean(validation$REMSN)
print(p-1.96*sqrt(p*(1-p)/306))
p = mean(test$REMSN)
print(p-1.96*sqrt(p*(1-p)/300))
	
# Original Logistic regression
glm.orig <- glm(REMSN~.-ID, family = binomial, data=train)
summary(glm.orig)
compare(glm.orig$fit, train$REMSN)
# training err = 31.17%
k<-predict(glm.orig, validation, type = "response")
compare(k, validation$REMSN) 
# validation err= 33.17%

# GLM with interaction
glm.inter <- glm(REMSN~.-ID+ssri*(L1SE+L1EFCCY+L1BEG+L2BEG+L1slope+MDSWCH1+MDAUG1), family = binomial, data=train)
summary(glm.inter)
compare(glm.inter$fit, train$REMSN)
# training err = 30.17%
k<-predict(glm.inter, validation, type = "response")
compare(k, validation$REMSN) 
# validation err= 34.31%

# glm with L1 penalty
X<-model.matrix(~.-ID-REMSN,data=train)
glm.l1<-glmnet(X, train$REMSN, family="binomial")
newx<-model.matrix(~.-ID-REMSN, data=validation)
pred<-predict(glm.l1, newx, type="response")
pred2<-predict(glm.l1, X, type="response")
err<-matrix(0, dim(pred)[2], 2)
for (i in 1:dim(pred)[2]){
	pred.s<-pred[, i]
	err[i, 1]<-compare(pred.s, validation$REMSN, T, F)$errorrate
	err[i, 2]<-compare(pred2[, i], train$REMSN, T, F)$errorrate
}
min(err[,1])
# validation: min(err) = 32.35% corresponding training error: 32.16%

# glm with L1 penalty & interaction
X<-model.matrix(~.-ID-REMSN+ssri*(L1SE+L1EFCCY+L1BEG+L2BEG+L1slope+MDSWCH1+MDAUG1),data=train)
glm.l1<-glmnet(X, train$REMSN, family="binomial")
newx<-model.matrix(~.-ID-REMSN, data=validation)
pred<-predict(glm.l1, newx, type="response")
pred2<-predict(glm.l1, X, type="response")
err<-matrix(0, dim(pred)[2], 2)
for (i in 1:dim(pred)[2]){
	pred.s<-pred[, i]
	err[i, 1]<-compare(pred.s, validation$REMSN, T, F)$errorrate
	err[i, 2]<-compare(pred2[, i], train$REMSN, T,  F)$errorrate
}
min(err[,1])
# validation: min(err) = 32.35% corresponding training error: 32.16%

# glm with L2 penalty
X<-model.matrix(~.-ID-REMSN,data=train)
glm.l2<-glmnet(X, train$REMSN, family="binomial", alpha=0)
newx<-model.matrix(~.-ID-REMSN, data=validation)
pred<-predict(glm.l2, newx, type="response")
pred2<-predict(glm.l2, X, type="response")
err<-matrix(0, dim(pred)[2], 2)
for (i in 1:dim(pred)[2]){
	pred.s<-pred[, i]
	err[i, 1]<-compare(pred.s, validation$REMSN, T, F)$errorrate
	err[i, 2]<-compare(pred2[, i], train$REMSN, T, F)$errorrate
}
min(err[,1])
# validation: min(err) = 32.35% corresponding training error: 31.83%

# glm with L2 penalty & interaction
X<-model.matrix(~.-ID-REMSN+ssri*(L1SE+L1EFCCY+L1BEG+L2BEG+L1slope+MDSWCH1+MDAUG1),data=train)
glm.l2<-glmnet(X, train$REMSN, family="binomial", alpha=0)
newx<-model.matrix(~.-ID-REMSN+ssri*(L1SE+L1EFCCY+L1BEG+L2BEG+L1slope+MDSWCH1+MDAUG1), data=validation)
pred<-predict(glm.l2, newx, type="response")
pred2<-predict(glm.l2, X, type="response")
err<-matrix(0, dim(pred)[2], 2)
for (i in 1:dim(pred)[2]){
	pred.s<-pred[, i]
	err[i, 1]<-compare(pred.s, validation$REMSN, T, F)$errorrate
	err[i, 2]<-compare(pred2[, i], train$REMSN, T, F)$errorrate
}
min(err[,1])
# validation: min(err) = 32.35% corresponding training error: 31.83%

# glm with 0.5*L1 + 0.5*L2 penalty
X<-model.matrix(~.-ID-REMSN,data=train)
glm.l1.5<-glmnet(X, train$REMSN, family="binomial", alpha=0.5)
newx<-model.matrix(~.-ID-REMSN, data=validation)
pred<-predict(glm.l1.5, newx, type="response")
pred2<-predict(glm.l1.5, X, type="response")
err<-matrix(0, dim(pred)[2], 2)
for (i in 1:dim(pred)[2]){
	pred.s<-pred[, i]
	err[i, 1]<-compare(pred.s, validation$REMSN, T, F)$errorrate
	err[i, 2]<-compare(pred2[, i], train$REMSN, T, F)$errorrate
}
min(err[,1])
# validation: min(err) = 32.35% corresponding training error: 33.67%

# glm with 0.5*L1 + 0.5*L2 penalty & interaction
X<-model.matrix(~.-ID-REMSN+ssri*(L1SE+L1EFCCY+L1BEG+L2BEG+L1slope+MDSWCH1+MDAUG1),data=train)
glm.l1.5<-glmnet(X, train$REMSN, family="binomial", alpha=0.5)
newx<-model.matrix(~.-ID-REMSN+ssri*(L1SE+L1EFCCY+L1BEG+L2BEG+L1slope+MDSWCH1+MDAUG1), data=validation)
pred<-predict(glm.l1.5, newx, type="response")
pred2<-predict(glm.l1.5, X, type="response")
err<-matrix(0, dim(pred)[2], 2)
for (i in 1:dim(pred)[2]){
	pred.s<-pred[, i]
	err[i, 1]<-compare(pred.s, validation$REMSN, T, F)$errorrate
	err[i, 2]<-compare(pred2[, i], train$REMSN, T, F)$errorrate
}
min(err[,1])
# validation: min(err) = 32.03% corresponding training error: 33.67%

# GLM with variable selection
glm.s <- glm(REMSN~SCHOOL+L2BEG+L1BEG+MDAUG1, family = binomial, data=train)
summary(glm.s)
compare(glm.s$fit, train$REMSN)
# training err = 31%
k<-predict(glm.s, validation, type = "response")
compare(k, validation$REMSN) 
# validation err = 34.64%

glm.rss <- regsubsets(REMSN~.-ID+ssri*(L1SE+L1EFCCY+L1BEG+L2BEG+L1slope+MDSWCH1+MDAUG1), family = binomial, data=train)

#glm with varibale selection DOESN'T WORK!!!
glm.s1<-update(glm.orig, .~.-HSPNC-dage)
summary(glm.s1)
compare(glm.s1$fit, train$REMSN)
# trainning error: 31.5%
k<-predict(glm.s1, validation, type = "response")
compare(k, validation$REMSN) 
# validation err= 33.99%

# Try PCA? Need More Work!
pca.comp<-prcomp(x=X)
pca.data<-cbind(train$REMSN, pca.comp$x[, 1:7])
colnames(pca.data)[1]<-"REMSN"
pca.data<-data.frame(pca.data)
glm.pca <- glm(REMSN~., family = binomial, data=pca.data)
summary(glm.pca)
k<-predict(glm.pca, validation, type = "response")
compare(k, validation$REMSN) 

# single classification tree
single.tree<-rpart(REMSN~.-ID, data=train, method = "class")
# minsplit, cp, maxdepth...
trainframe<-data.frame(model.matrix(~.-REMSN,data=train))
pred <- predict(single.tree, type = "prob")[,2]
compare(pred, train$REMSN) #training set error: 25%
pred <- predict(single.tree,validation, type = "prob")[,2]
compare(pred, validation$REMSN)
# validation error: 34.97%

single.tree2<-rpart(REMSN~.-ID, data=train, method = "class", minsplit = 10, cp=0.005)
# minsplit, cp, maxdepth...
pred <- predict(single.tree2, type = "prob")[,2]
compare(pred, train$REMSN) #training set error: 17.17%
pred <- predict(single.tree2,validation, type = "prob")[,2]
compare(pred, validation$REMSN)
# validation error: 34.64%

single.tree3<-rpart(REMSN~.-ID, data=train, method = "class", minsplit = 8, cp=0.0025)
# minsplit, cp, maxdepth...
pred <- predict(single.tree3, type = "prob")[,2]
compare(pred, train$REMSN) #training set error: 11.83%
pred <- predict(single.tree3,validation, type = "prob")[,2]
compare(pred, validation$REMSN)
# validation error: 40.85%

# random Forest
X<-model.matrix(~.-ID-REMSN,data=train)
newx<-model.matrix(~.-ID-REMSN, data=validation)
rf <- randomForest(X,y=as.factor(train$REMSN), xtest = newx, ytest = as.factor(validation$REMSN), keep.forest = T)
#OOB 37.83%
pred<-predict(rf, newdata=X)
compare(pred, train$REMSN) 
#training error = 0%
pred<-predict(rf, newdata=newx)
compare(pred, validation$REMSN) 
#training error = 32.68%

# random Forest using interaction
X<-model.matrix(~.-ID-REMSN+ssri*(L1SE+L1EFCCY+L1BEG+L2BEG+L1slope+MDSWCH1+MDAUG1),data=train)
newx<-model.matrix(~.-ID-REMSN+ssri*(L1SE+L1EFCCY+L1BEG+L2BEG+L1slope+MDSWCH1+MDAUG1), data=validation)
rf <- randomForest(X,y=as.factor(train$REMSN), xtest = newx, ytest = as.factor(validation$REMSN), keep.forest = T)
#OOB 37.17%
pred<-predict(rf, newdata=X)
compare(pred, train$REMSN) 
#training error = 0%
pred<-predict(rf, newdata=newx)
compare(pred, validation$REMSN) 
#training error = 31.7%
newxt<-model.matrix(~.-ID-REMSN+ssri*(L1SE+L1EFCCY+L1BEG+L2BEG+L1slope+MDSWCH1+MDAUG1), data=test)
pred<-predict(rf, newdata=newxt)
compare(pred, test$REMSN) 
# test error 30.33% 

# gbm
gbm1<-gbm(REMSN~.-ID, data=train, distribution = "bernoulli", n.trees = 5000)
pred<-predict(gbm1, newdata=train, n.trees = 5000, type = "response")
compare(pred, train$REMSN) 
#training error = 30.5%
pred<-predict(gbm1, newdata=validation, n.trees = 5000, type = "response")
compare(pred, validation$REMSN) 
# validation error = 33.33%

# svm
svm1<-svm(REMSN~.-ID, data=train)
pred <- predict(svm1, newdata=train)
compare(pred, train$REMSN) #training error: 17.67%
pred <- predict(svm1, newdata=validation)
compare(pred, validation$REMSN) # validation error = 31.7%
pred <- predict(svm1, newdata=test)
compare(pred, test$REMSN) # test error = 29.33%

# neural net
formula = REMSN~MARITB+EMPLB+SCHOOL+MKEDC+ENJOY+FAMIM+SEX+HSPNC+WHITE+BLACK+age+dage+EPLT5+episode_date+ATMTSUIC+L1SE+L1EFCCY+MDSWCH1+MDAUG1+ssri+L2BEG+L1BEG+L1slope
trainf<-train
trainf$REMSN <-as.factor(trainf$REMSN)
nnet1<-nnet(formula, data=trainf, size = 3, maxit = 1000)
pred <- predict(nnet1, newdata=train) 
compare(pred, train$REMSN) #training error: 26%
pred <- predict(nnet1, newdata=validation)
compare(pred, validation$REMSN) # validation error = 35.62%

dump(c("glm.orig", "glm.l1", "glm.l1.5", "glm.l2"), "model.R")