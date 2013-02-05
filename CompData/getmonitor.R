getmonitor<- function(id,directory, summarize=FALSE){
	while(nchar(id)<3){
		id<-paste("0", id, sep="")
	}
	data<-read.csv(paste(getwd(),"/",directory, "/", id, ".csv", sep=""))
	if(summarize){summary(data)}
	data
}