complete<-function(directory, id=1:332){
	m <- length(id)
	obs<-rep(0, m)
	for (i in 1:m){
		data<- getmonitor(id[i], directory)
		obs[i]<-sum((!is.na(data$nitrate))&(!is.na(data$sulfate)))
	}
	k<-cbind(id, obs)
	k<-data.frame(k)
	colnames(k)<-c("id", "nobs")
	k
}