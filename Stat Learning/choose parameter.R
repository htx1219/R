library(caret)

# random Forest using interaction
X<-model.matrix(~.-ID-REMSN+ssri*(L1SE+L1EFCCY+L1BEG+L2BEG+L1slope+MDSWCH1+MDAUG1),data=train)
newx<-model.matrix(~.-ID-REMSN+ssri*(L1SE+L1EFCCY+L1BEG+L2BEG+L1slope+MDSWCH1+MDAUG1), data=validation)
rf <- randomForest(X,y=as.factor(train$REMSN), xtest = newx, ytest = as.factor(validation$REMSN), keep.forest = T, ntree = 500, mtry=5)
#OOB 37.17%
#training error = 0%
pred<-predict(rf, newdata=newx)
compare(pred, validation$REMSN) 
#validation error = 31.05% or 30.72% or 31.37%
newxt<-model.matrix(~.-ID-REMSN+ssri*(L1SE+L1EFCCY+L1BEG+L2BEG+L1slope+MDSWCH1+MDAUG1), data=test)
pred<-predict(rf, newdata=newxt)
compare(pred, test$REMSN) 
# test error 29.67% or 29.33% 
rf.save = rf
dump("rf.save", "rf.R")
pred<-predict(rf.save, newdata=newx)
compare(pred, validation$REMSN) 
# validation error = 29.74%
# Among 92 positive cases, 58 are real positive, 34 are false positive, and the false positive rate is 36.96 %
# Among 214 negative cases, 158 are real negative, 56 are false negative, and the false negative rate is 26.17 %
# The classification error rate is 29.41 %
# The Chi-square value for McNemar test is 4.9 , and the p value is 0.0269 

pred<-predict(rf.save, newdata=newxt)
compare(pred, test$REMSN) #test error = 29.67%

allrf<-list()
err<-rep(0, 100)
for (i in 1:100){
	allrf[[i]] <- randomForest(X,y=as.factor(train$REMSN), xtest = newx, ytest = as.factor(validation$REMSN), keep.forest = T, ntree = 500)
#OOB 37.17%
#training error = 0%
pred<-predict(allrf[[i]], newdata=newx)
err[i]<-compare(pred, validation$REMSN, T, F)$errorrate
}
min(err)


# gbm
gbm1<-gbm(REMSN~.-ID+ssri*(L1SE+L1EFCCY+L1BEG+L2BEG+L1slope+MDSWCH1+MDAUG1), data=train, distribution = "bernoulli", shrinkage = 0.1, n.trees = 200)
pred<-predict(gbm1, newdata=train, n.trees = 400, type = "response")
compare(pred, train$REMSN) 
#training error = 30.5%
pred<-predict(gbm1, newdata=validation, n.trees = 400, type = "response")
compare(pred, validation$REMSN) 
# validation error = 33.33%
t <- rep(400, 100)
err <- matrix(0, length(t), 2)
gbm.t<-list()
for (i in 1:length(t)){
	gbm.t[[i]] <- gbm1<-gbm(REMSN~.-ID, data=train, distribution = "bernoulli", shrinkage = 0.1, n.trees = t[i])
	pred2 <- predict(gbm.t[[i]], newdata=train, n.trees = t[i], type = "response")
	pred.s <-  predict(gbm.t[[i]], newdata=validation, n.trees = t[i], type = "response")
	err[i, 1]<-compare(pred.s, validation$REMSN, T, F)$errorrate
	err[i, 2]<-compare(pred2, train$REMSN, T, F)$errorrate
}
min(err[,1])
which(err[,1] == min(err[,1]))

gbm.save<-gbm.t[[35]]

pred<-predict(gbm.save, newdata=train, n.trees = 400, type = "response")
compare(pred, train$REMSN) 
pred<-predict(gbm.save, newdata=validation, n.trees = 400, type = "response")
compare(pred, validation$REMSN) # validation 30.39%, train:23.83%
# Among 91 positive cases, 56 are real positive, 35 are false positive, and the false positive rate is 38.46 %
# Among 215 negative cases, 157 are real negative, 58 are false negative, and the false negative rate is 26.98 %
# The classification error rate is 30.39 %
pred<-predict(gbm.save, newdata=test, n.trees = 400, type = "response")
compare(pred, test$REMSN) #test error 30.67%
dump(c("gbm.save"), "gbm.R")

# svm
svm1<-svm(REMSN~.-ID, data=train, type = "C-classification", cost = 0.625, probability=T)
pred <- predict(svm1, newdata=train, probability=T)
compare(pred, train$REMSN) #training error: 22.67%
pred <- predict(svm1, newdata=validation)
compare(pred, validation$REMSN) # validation error = 31.37%
# Among 80 positive cases, 49 are real positive, 31 are false positive, and the false positive rate is 38.75 %
# Among 226 negative cases, 161 are real negative, 65 are false negative, and the false negative rate is 28.76 %
# The classification error rate is 31.37 %
pred <- predict(svm1, newdata=test)
compare(pred, test$REMSN) # test error = 29.67%

svm1<-svm(REMSN~.-ID, data=train, cost = 0.58)
pred <- predict(svm1, newdata=train)
compare(pred, train$REMSN) #training error: 23.67%
pred <- predict(svm1, newdata=validation)
compare(pred, validation$REMSN) # validation error = 31.37%
pred <- predict(svm1, newdata=test)
compare(pred, test$REMSN) # test error = 30%

allsvm<-list()
#cost<-c(0.01, 0.03, 0.1, 0.3, 0.5, 0.8, 1, 1.5, 2, 3, 10, 30)
cost<-seq(0.5, 0.7, 0.001)
err<-matrix(0, length(cost), 2)
for (i in 1:length(cost)){
	allsvm[[i]]<-svm(REMSN~.-ID, data=train, cost = cost[i], type = "C-classification")
	pred2 <- predict(allsvm[[i]], newdata=train)
	pred.s <-  predict(allsvm[[i]], newdata=validation)
	err[i, 1]<-compare(pred.s, validation$REMSN, T, F)$errorrate
	err[i, 2]<-compare(pred2, train$REMSN, T, F)$errorrate
}
min(err[, 1])