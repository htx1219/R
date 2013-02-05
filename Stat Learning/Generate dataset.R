set.seed(1219)
real<-read.csv("real.csv")
train.sample <- sample(1206, 600,replace = F)
validation.sample<-sample(606, 306, replace = F)
train.sample<-sort(train.sample)
validation.sample<-sort(validation.sample)
library(pls)
c(F, F, F, T, T, T, T, F, F, F, F, T, T, F, T, F, F, F, F, F, F, T, T, T, F)
real.c<-real[, c(4, 5, 6, 7, 12, 13, 15, 22, 23, 24)]
real.c<- scale(real.c, center = T, scale = T)
real.n <- real
real.n[, c(4, 5, 6, 7, 12, 13, 15, 22, 23, 24)] <- real.c

train<-real[train.sample,]
other<-real[-train.sample,]
validation<-other[validation.sample,]
test<-other[-validation.sample,]

real.l<-real
real.l[, 15]<-log(-real[,15])
train<-real.n[train.sample,]
other<-real.n[-train.sample,]
validation<-other[validation.sample,]
test<-other[-validation.sample,]

train<-real.l[train.sample,]
other<-real.l[-train.sample,]
validation<-other[validation.sample,]
test<-other[-validation.sample,]
dump(c("train", "test", "validation"),"datasetl.R")

library(tsne)
colors = rainbow(length(unique(real.l$REMSN)))
names(colors) = unique(real.l$REMSN)
ecb = function(x,y){ plot(x,t='n'); text(x,labels=real.l$REMSN, col=colors[real.l$REMSN+1]) }
tsne_stard = tsne(real.l[,2:24], epoch_callback = ecb, perplexity=50)
colnames(tsne_stard)<-c("TSNE Dimension 1", "TSNE Dimension 2")
plot(tsne_stard, t='n', main = "TSNE plot")
text(tsne_stard, labels = real.l$REMSN, col = colors[real.l$REMSN+1])

# compare to PCA
dev.new()
real.centered<-scale(real.l[,2:24], center=T, scale = T)
w = princomp(real.centered)
summary(w)
pca_stard = princomp(real.l[,2:24])$scores[,1:2] # Only after comp 13.. So give up!
plot(pca_stard, t='n', xlab = "PC 1", ylab = "PC 2", main="PCA plot\n Proportion of Variance: 87%")
text(pca_stard, labels=real.l$REMSN,col=colors[real.l$REMSN+1])