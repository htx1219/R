rankhospital<-function(state, outcome, num){
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
		p = 11
	}
	if(outcome == "heart failure"){
p = 17
	}
	if(outcome == "pneumonia"){
p = 23
	}
		k2<-k[k$State == state,]
		rate<-k2[,p]
		rate<-sort(rate)
		if(num == "best"){
			num = 1
		}
		if(num == "worst"){
			num = length(rate)
		}
		r<-rate[num]
		w<-k2$Hospital.Name[which(k2[,p] == r)]
		w<-sort(w)
		nless = sum(k2[,p] < r, na.rm=T)
		return(w[num-nless])
}