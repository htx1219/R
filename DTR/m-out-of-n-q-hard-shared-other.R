para.generation <- function(){
gamma<-numeric(7)
f<-k<-numeric(4)
delta<-runif(2)
delta<-round(delta,digits=4)
cat("delta = ", delta, "\n")
k[1]<-0.25*expit(delta[1]+delta[2])
k[2]<-0.25*expit(-delta[1]+delta[2])
k[3]<-0.25*expit(delta[1]-delta[2])
k[4]<-0.25*expit(-delta[1]-delta[2])
k<-round(k,digits=4)
cat("k = ", k, "\n")
gamma[5:7]<- rnorm(3)
f[1]<-gamma[5]+gamma[6]+gamma[7]
f[2]<-gamma[5]+gamma[6]-gamma[7]
f[3]<-gamma[5]-gamma[6]+gamma[7]
f[4]<-gamma[5]-gamma[6]-gamma[7]
f<-round(f,digits=4)
cat("f = ", f, "\n")
gamma[3]<-gamma[5]-(k[1]+k[2])*(abs(f[1])-abs(f[4]))+(k[3]+k[4])*(abs(f[2])-abs(f[3]))
gamma[4]<-gamma[6]-(k[1]-k[2])*(abs(f[1])-abs(f[3]))+(k[3]-k[4])*(abs(f[2])-abs(f[4]))
gamma<-round(gamma,digits=4)
cat("gamma = ", gamma, "\n")
gamma.delta<-c(gamma,delta)
return (gamma.delta)
}

data.generation <- function(gamma, delta, N){ 
	
	full.data <- NULL
      O <- matrix(0,N,2)
	temp <- 2*matrix(rbinom(N*3, 1, 0.5),N,3)-1
	A <- temp[,c(1:2)]
	O[,1] <- temp[,3]
	p <- expit(delta[1]*O[,1]+delta[2]*A[,1])	
	O[,2] <- 2*matrix(rbinom(N,1,p),N,1)-1
	
	
	mu <- gamma[1] + gamma[2]*O[,1] + gamma[3]*A[,1] + gamma[4]*O[,1]*A[,1] + gamma[5]*A[,2] + gamma[6]*O[,2]*A[,2] + gamma[7]*A[,1]*A[,2]
	Y <- rnorm(N,0,1)
	
	full.data$Y <- Y + mu
	full.data$A <- A
	full.data$O <- O
	
	data.matrix<-cbind(full.data$Y, A, O)
	
	return(data.matrix)
}

choose.m.pretest <- function(data, Psi, alpha=0.1){
	
	nu <- 0.001     # can be varied
	
	Y <- data[,1]
	A <- data[,2:3]
	O <- data[,4:5]
	
	n <- length(Y)
	
	X2 <- cbind(1, O[,1], A[,1], O[,1]*A[,1], A[,2], O[,2]*A[,2], A[,1]*A[,2])
	beta.psi.2 <- Psi[1:7]
	
	stage2dim <- length(beta.psi.2)
	
	Yprime <- beta.psi.2[5] + beta.psi.2[6]*O[,2] + beta.psi.2[7]*A[,1]

	Sigma2 <- n*solve(t(X2)%*%X2)
	
	Z2 <- (diag(array(Y - X2%*%matrix(beta.psi.2,stage2dim,1)))%*%X2%*%Sigma2)/sqrt(n-7)

    Cov2 <- t(Z2)%*%Z2
    Sigma.stage2 = Cov2[c(5:7),c(5:7)]
    h<-cbind(1, O[,2], A[,1])
    TS <- abs(Yprime)/sqrt(diag(h%*%Cov2[c(5:7),c(5:7)]%*%t(h))/n)

    nonregularity <- (TS <= qnorm(1-nu/2))
    p <- mean(nonregularity)
    
    m <- n^(1-p*alpha/(alpha+1))    # this is actually n^((1+alpha(1-p))/(1+alpha)) with alpha=0.1
    m <- ceiling(m)
	return(m)
	}
	
choose.m.same.as.n <- function(data, Psi=0, alpha=0){
	Y <- data[,1]
	n <- length(Y)
	return(n)
	}
	
calc.pseudooutcome <- function(sim.data,Beta.Psi=NULL){
	
	Y <- sim.data$Y 
	A <- sim.data$A 
	O <- sim.data$O	
		
	beta.psi.2 <- NULL
	if (!is.null(Beta.Psi)){beta.psi.2 <- Beta.Psi}
	else{
		X2 <- cbind(1, O[,1], A[,1], O[,1]*A[,1], A[,2], O[,2]*A[,2], A[,1]*A[,2])
		beta.psi.2 <- as.numeric(coef(lm(Y~X2-1)))
		}
	
	Yprimeprime <- beta.psi.2[5] + beta.psi.2[6]*O[,2] + beta.psi.2[7]*A[,1]
	
	Y.tilde <- beta.psi.2[1] +  beta.psi.2[2]*O[,1] +  beta.psi.2[3]*A[,1] +  beta.psi.2[4]*A[,1]*O[,1] + abs(Yprimeprime)
	
    return(Y.tilde)	
	}



# Tianxiao: the following 'q.hard.shared.est' is the shared hardmax procedure (uses 'qr.solve' function)

q.hard.shared.est <- function(sim.data, err.tol, maximum.iter=500){
	
	# array of psi values at each iteration
	results <- list()
	
	Y <- sim.data$Y 
	A <- sim.data$A 
	O <- sim.data$O
	
	Y.tilde <- calc.pseudooutcome(sim.data,Beta.Psi=NULL)
	
	Y.star <- c(Y, Y.tilde)
	Z <- rbind(cbind(1, O[,1], A[,1], O[,1]*A[,1], A[,2], O[,2]*A[,2], A[,1]*A[,2], 0, 0), cbind(0, 0, 0, 0, A[,1], A[,1]*O[,1], 0, 1, O[,1]))  # initial value of the matrix
	
     	Beta.Psi <- qr.solve(Z,Y.star)
	Y.star <- matrix(Y.star,,1)
	
	output.vec <- matrix(Beta.Psi[c(5:6)],1,2)
	
	output.vec.prev <- output.vec*1000  # just for initialization
	total.iter <- 1
	
	results$Psi0[[total.iter]] <- output.vec[1]
	results$Psi1[[total.iter]] <- output.vec[2]
	results$Psi<-Beta.Psi
	
	while ((max(abs(output.vec-output.vec.prev)) > err.tol) & (total.iter<=maximum.iter)){
		total.iter <- total.iter + 1
		output.vec.prev <- output.vec
		Y.tilde <- calc.pseudooutcome(sim.data,Beta.Psi)
		Y.star <- matrix(c(Y, Y.tilde),,1)
		Beta.Psi <- qr.solve(Z,Y.star)
		output.vec <- matrix(Beta.Psi[c(5:6)],1,2)
		
		results$Psi0[[total.iter]] <- output.vec[1]
	    results$Psi1[[total.iter]] <- output.vec[2]
	    results$Psi<-Beta.Psi
		}
	if (total.iter > maximum.iter) {print('Maximum iterations reached in q.hard.shared')}
	
	results$total.iter <- total.iter
	results$output.vec <- output.vec
	results$Psi<-Beta.Psi
	
	return(results)
}

nonsmooth.min.est <- function(sim.data, err.tol, maximum.iter=500){
	
	# array of psi values at each iteration
	results <- list()
	
	Y <- sim.data$Y 
	A <- sim.data$A 
	O <- sim.data$O
	
	Y.tilde <- calc.pseudooutcome(sim.data,Beta.Psi=NULL)
	
	Y.star <- c(Y, Y.tilde)
	Z <- rbind(cbind(1, O[,1], A[,1], O[,1]*A[,1], A[,2], O[,2]*A[,2], A[,1]*A[,2], 0, 0), cbind(0, 0, 0, 0, A[,1], A[,1]*O[,1], 0, 1, O[,1]))  # initial value of the matrix
	
      residual<-function(theta){
      r<-Y.star-Z%*%theta
      t(r)%*%r
      }      

	Beta.Psi <- nlminb(c(0,0,0,0,0,0,0,0,0),residual)$par
	Y.star <- matrix(Y.star,,1)
	
	output.vec <- matrix(Beta.Psi[c(5:6)],1,2)
	
	output.vec.prev <- output.vec*1000  # just for initialization
	total.iter <- 1
	
	results$Psi0[[total.iter]] <- output.vec[1]
	results$Psi1[[total.iter]] <- output.vec[2]
	
	while ((max(abs(output.vec-output.vec.prev)) > err.tol) & (total.iter<=maximum.iter)){
		total.iter <- total.iter + 1
		output.vec.prev <- output.vec
		Y.tilde <- calc.pseudooutcome(sim.data,Beta.Psi)
		Y.star <- matrix(c(Y, Y.tilde),,1)
		Beta.Psi <- nlminb(Beta.Psi,residual)$par
		output.vec <- matrix(Beta.Psi[c(5:6)],1,2)
		
		results$Psi0[[total.iter]] <- output.vec[1]
	    results$Psi1[[total.iter]] <- output.vec[2]
		}
	if (total.iter > maximum.iter) {print('Maximum iterations reached in q.hard.shared')}
	
	results$total.iter <- total.iter
	results$output.vec <- output.vec
	results$Psi<-Beta.Psi
	
	return(results)
}


# Tianxiao: the following 'nonlinear.eqa.est' is a third variation (uses 'multiroot' function) -- the goal is to investigate if these various solving/optimization procedures make any difference

library(rootSolve)
nonlinear.eqa.est <- function(sim.data, err.tol, maximum.iter=500){
     
	# array of psi values at each iteration
	results <- list()
	
	Y <- sim.data$Y 
	A <- sim.data$A 
	O <- sim.data$O
	
	Y.tilde <- calc.pseudooutcome(sim.data,Beta.Psi=NULL)
	
	Y.star <- c(Y, Y.tilde)
	Z <- rbind(cbind(1, O[,1], A[,1], O[,1]*A[,1], A[,2], O[,2]*A[,2], A[,1]*A[,2], 0, 0), cbind(0, 0, 0, 0, A[,1], A[,1]*O[,1], 0, 1, O[,1]))  # initial value of the matrix
	
      equation<-function(theta){
      r<-Y.star-Z%*%theta
      t(Z)%*%r
      }      

	Beta.Psi <- multiroot(equation,start=c(0,0,0,0,0,0,0,0,0))$root
	Y.star <- matrix(Y.star,,1)
	
	output.vec <- matrix(Beta.Psi[c(5:6)],1,2)
	
	output.vec.prev <- output.vec*1000  # just for initialization
	total.iter <- 1
	
	results$Psi0[[total.iter]] <- output.vec[1]
	results$Psi1[[total.iter]] <- output.vec[2]
	
	while ((max(abs(output.vec-output.vec.prev)) > err.tol) & (total.iter<=maximum.iter)){
		total.iter <- total.iter + 1
		output.vec.prev <- output.vec
		Y.tilde <- calc.pseudooutcome(sim.data,Beta.Psi)
		Y.star <- matrix(c(Y, Y.tilde),,1)
		Beta.Psi <- multiroot(equation,start=Beta.Psi)$root
		output.vec <- matrix(Beta.Psi[c(5:6)],1,2)
		
		results$Psi0[[total.iter]] <- output.vec[1]
	    results$Psi1[[total.iter]] <- output.vec[2]
		}
	if (total.iter > maximum.iter) {print('Maximum iterations reached in q.hard.shared')}
	
	results$total.iter <- total.iter
	results$output.vec <- output.vec
	results$Psi<-Beta.Psi
	
	return(results)
}

q.hard.averaged <- function(sim.data, err.tol=0){
	
	# array of psi values at each iteration
	results <- list()
	
	Y <- sim.data$Y 
	A <- sim.data$A 
	O <- sim.data$O
	
	X2 <- cbind(1, O[,1], A[,1], O[,1]*A[,1], A[,2], O[,2]*A[,2], A[,1]*A[,2])
	X1 <- cbind(1, O[,1], A[,1], A[,1]*O[,1])
		
	beta.psi.2 <- as.numeric(coef(lm(Y ~ X2-1)))
	
	stage2dim <- length(beta.psi.2)
	
	H20 <- cbind(1, O[,1], A[,1], O[,1]*A[,1])
	H21 <- cbind(1, O[,2], A[,1])
	
	Y.star <- H20%*%beta.psi.2[c(1:ncol(H20))]+abs(H21%*%beta.psi.2[c((ncol(H20)+1):stage2dim)])
	
	beta.psi.1 <- as.numeric(coef(lm(Y.star ~ X1-1)))
	
	results$Psi1<-beta.psi.1
	results$Psi2<-beta.psi.2
	
	results$Psi[1:4] <- beta.psi.2[1:4]
	results$Psi[5:6] <- (beta.psi.2[5:6]+beta.psi.1[3:4])/2
	results$Psi[7] <- beta.psi.2[7]
	results$Psi[8:9] <- beta.psi.1[1:2]
	return(results)
}