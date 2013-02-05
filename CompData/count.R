count<-function(cause=NULL){
	if (length(cause) == 0){
		stop("give me a cause!")
	}
	homicides<-readLines("homicides.txt")
	if (cause == "shooting"){
		return(length(grep("Cause: [Ss]hooting", homicides)))
	}else if (cause == "asphyxiation"){
		return(length(grep("Cause: [Aa]sphyxiation", homicides)))
	}else if (cause == "blunt force"){
		return(length(grep("Cause: [Bb]lunt [Ff]orce", homicides)))
	}else if (cause == "other"){
		return(length(grep("Cause: [Oo]ther", homicides)))
	}else if (cause == "stabbing"){
		return(length(grep("Cause: [Ss]tabbing", homicides)))
	}else if (cause == "unknown"){
		return(length(grep("Cause: [Uu]nknown", homicides)))
	}else {
		stop("invalid cause")
	}
	
}