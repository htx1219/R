outcome<-read.csv("outcome-of-care-measures.csv", colClasses="character")
head(outcome)
outcome[,11]<-as.numeric(outcome[,11])
hist(outcome[,11], main = "Heart Attack 30-day Death Rate", xlab="30-day Death Rate")

outcome[,11]<-as.numeric(outcome[,11])
outcome[,17]<-as.numeric(outcome[,17])
outcome[,23]<-as.numeric(outcome[,23])
par(mfrow=c(3,1))

hist(outcome[,11], main = substitute("Heart Attack(" * bar(x)  * "="* k * ")", list(k =mean(outcome[,11], na.rm=T))), xlab="30-day Death Rate", xlim=c(6,22), prob=T)
abline(v=median(outcome[,11], na.rm=T))
d<-density(outcome[,11], na.rm=T)
lines(d)

hist(outcome[,17], main = "Heart Failure", xlab="30-day Death Rate",xlim=c(6,22))
abline(v=median(outcome[,17], na.rm=T))
hist(outcome[,23], main = "Pneumonia", xlab="30-day Death Rate",xlim=c(6,22))
abline(v=median(outcome[,23], na.rm=T))

y<-table(outcome$State)
outcome2<-outcome[y[outcome$State] >20,]
death<-outcome2[,11]
state<-outcome2$State
boxplot(death~state)

library(lattice)
hospital<-read.csv("hospital-data.csv", colClasses="character")
outcome.hospital<-merge(outcome, hospital, by="Provider.Number")
death<-as.numeric(outcome.hospital[,11])
npatient<-as.numeric(outcome.hospital[,15])
owner<-factor(outcome.hospital$Hospital.Ownership)
xyplot(death~npatient|owner, xlab="Number of Patients Seen", ylab="30-dat Death Rate", main="Heart Attack 30-day Death Rate by Ownership", panel = function(x, y,...){
	panel.xyplot(x, y, ...)
	panel.lmline(x,y, col=2)
})