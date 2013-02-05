best<-function(state, outcome){
	k<-read.csv("outcome-of-care-measures.csv", colClasses="character")
	k[,11]<-as.numeric(k[,11])
	k[,17]<-as.numeric(k[,17])
	k[,23]<-as.numeric(k[,23])
	if(!state %in% dimnames(table(k$State))[[1]]){
		stop("invalid state")
	}
	if(!outcome %in% c("heart failure", "heart attack", "pneumonia")){
		stop("invalid outcome")
	}
	if(outcome == "heart attack"){
		k2<-k[k$State == state,]
		w<-k2$Hospital.Name[which(k2[,11] == min(k2[,11], na.rm=T))]
		w<-sort(w)
		return(w[1])
	}
	if(outcome == "heart failure"){
		k2<-k[k$State == state,]
		w<-k2$Hospital.Name[which(k2[,17] == min(k2[,17], na.rm=T))]
		w<-sort(w)
		return(w[1])
	}
	if(outcome == "pneumonia"){
		k2<-k[k$State == state,]
		w<-k2$Hospital.Name[which(k2[,23] == min(k2[,23], na.rm=T))]
		w<-sort(w)
		return(w[1])
	}
}