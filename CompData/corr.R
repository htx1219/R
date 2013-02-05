corr<-function(directory, threshold=0){
	obs<-complete(directory)
	w<-obs$id[obs$nobs>threshold]
	getcor<-function(id){
		data<-getmonitor(id,directory)
		cor(data$sulfate, data$nitrate, use="complete.obs")
		}
	sapply(w, getcor)
}