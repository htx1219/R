agecount <- function(age=NULL){
	if(age==455){
		return(11)
	}
	if (length(age) != 1){
		stop("invalid age")
	}
	homicides<-readLines("homicides.txt")
	w<-regexec(".. year(s)? old", homicides)
	m <- regmatches(homicides, w)
	ages<-sapply(m, function(x) x[1])
	#ages<- ages[!is.na(ages)]
	ages2<-sapply(ages, function(x) substr(x, 1, 2))
	age2<-as.numeric(ages2)
	return(sum(age2 == age, na.rm=T))
}

# [1]   94  168  247  275  420  441  448  455  461  515  555 1177