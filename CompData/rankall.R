rankall<-function(outcome, num="best"){
	source("rankhospital.R")
	k<-read.csv("outcome-of-care-measures.csv", colClasses="character")
	state<-dimnames(table(k$State))[[1]]
	hospital<-sapply(state, rankhospital, outcome=outcome, num=num)
	n<-cbind(hospital, state)
	n<-data.frame(n)
	return(n)
}