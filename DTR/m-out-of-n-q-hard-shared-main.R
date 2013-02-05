set.seed(18)
source('m-out-of-n-q-hard-shared-other.R')
library(rootSolve)

options(digits=5)
n <- 300
simsize <- 1000     
kboot <- 1000      # no. of bootstrap samples
err.tol <- 0.001

q.hard.shared.changed <- q.hard.shared.est
# q.hard.shared.changed <- nonsmooth.min.est
# q.hard.shared.changed <- nonlinear.eqa.est
# q.hard.shared.changed <- q.hard.averaged

# choose.m.changed <- choose.m.pretest
choose.m.changed <- choose.m.same.as.n
alpha = 0.025


# Ex 1 (psi0 = 0 = psi1):
# gamma <- c(0,0,0,0,0,0,0)
# delta <- c(.5,.5)

#Ex 2 (psi0 = 0.5, psi1 = 0):
# gamma <- c(0,0,0,0,0.5,0,0.5)
# delta <- c(.5,.5)

#Ex 3 (psi0 = 1, psi1 = 0.5):
# gamma <- c(0,0,0.5,0.5,1,0.5,0.5)
# delta <- c(1,0)

#Ex 4 (psi0 = 1, psi1 = 1):
 gamma <- c(0,0,0.5,0.77,1,1,1)
 delta <- c(1,0)

expit <- function(x) { exp(x)/(1+exp(x)) }


# Parameters to perform inference on are psi_{10} and psi_{11}, where

# psi.10 <- gamma[3] + 0.25*(expit(delta[1] + delta[2]) + expit(-delta[1] + delta[2]))*abs(gamma[5] + gamma[6] + gamma[7]) - 0.25*(expit(delta[1] - delta[2]) + expit(-delta[1] - delta[2]))*abs(gamma[5] + gamma[6] - gamma[7]) + 0.25*(expit(delta[1] - delta[2]) + expit(-delta[1] - delta[2]))*abs(gamma[5] - gamma[6] + gamma[7]) - 0.25*(expit(delta[1] + delta[2]) + expit(-delta[1] + delta[2]))*abs(gamma[5] - gamma[6] - gamma[7])

# psi.11 <- gamma[4] + 0.25*(expit(delta[1] + delta[2]) - expit(-delta[1] + delta[2]))*abs(gamma[5] + gamma[6] + gamma[7]) - 0.25*(expit(delta[1] - delta[2]) - expit(-delta[1] - delta[2]))*abs(gamma[5] + gamma[6] - gamma[7]) - 0.25*(expit(delta[1] + delta[2]) - expit(-delta[1] + delta[2]))*abs(gamma[5] - gamma[6] + gamma[7]) + 0.25*(expit(delta[1] - delta[2]) - expit(-delta[1] - delta[2]))*abs(gamma[5] - gamma[6] - gamma[7])

psi.10 = gamma[5]
psi.11 = gamma[6]
data <- data.generation(gamma, delta, n*simsize)

                                
# initialize final quantities of interest

# bias
bias.hard <- matrix(0,1,2)

# variance
est.squared.hard <- matrix(0,1,2)

# coverage of 95% CIs
cover.95.hard.percentile <- matrix(0,1,2)
cover.95.hard.hybrid <- matrix(0,1,2)

# coverage of 90% CIs
cover.90.hard.percentile <- matrix(0,1,2)
cover.90.hard.hybrid <- matrix(0,1,2)

# bootstrap data
boot.est.hard = matrix(0, kboot, 2)

# chosen value of m
m.chosen = matrix(0,simsize,1)

#i<-1

for (i in 1:simsize){
	print(i)
    lower <- (i-1)*n + 1
    upper <- i*n
    data1 <- data[c(lower:upper),]
    Y <- data1[,1]
    A <- data1[,c(2:3)]
    O <- data1[,c(4:5)]
    
    sim.data<-NULL
    sim.data$Y <- Y
    sim.data$A <- A
    sim.data$O <- O
    
    # hardmax estimator
    qr.result <- q.hard.shared.changed(sim.data, err.tol)$Psi
    main.int.hard<-qr.result[5:6]
    bias.hard <- bias.hard + main.int.hard
    est.squared.hard <- est.squared.hard + main.int.hard*main.int.hard
    
    # bootstrap
    
    m <- choose.m.changed(data1, qr.result, alpha)
    m.chosen[i] <- m
    
    bootsam<- matrix(sample(n, kboot*m, replace=T), m, kboot)

    for (b in 1:kboot){
        index.b <- bootsam[,b]
        Y.temp <- Y[index.b]
        A.temp <- A[index.b,]
        O.temp <- O[index.b,]
        
        sim.data<-NULL
    	sim.data$Y <- Y.temp
    	sim.data$A <- A.temp
    	sim.data$O <- O.temp
    
    	# hardmax estimator
    	qr.result <- q.hard.shared.changed(sim.data, err.tol)$Psi
        
        boot.est.hard[b,] <- qr.result[5:6]
        
     }
    
    # bootstrap quantiles 
    # 2-by-2 matrix, rows for quantiles, columns for (main and interaction) effects
    
    # calculating the coverage of 95% CIs
    quan.95.hard <- cbind(quantile(boot.est.hard[,1], c(0.025,0.975)),quantile(boot.est.hard[,2], c(0.025,0.975)))
    
    cover.95.hard.percentile <- cover.95.hard.percentile + c((quan.95.hard[1,1] <= psi.10)&(quan.95.hard[2,1] >= psi.10),(quan.95.hard[1,2] <= psi.11)&(quan.95.hard[2,2] >= psi.11))
    
   
    cover.95.hard.hybrid <- cover.95.hard.hybrid + c((2*main.int.hard[1]-quan.95.hard[2,1] <= psi.10)&(2*main.int.hard[1]-quan.95.hard[1,1] >= psi.10),
                                             (2*main.int.hard[2]-quan.95.hard[2,2] <= psi.11)&(2*main.int.hard[2]-quan.95.hard[1,2] >= psi.11))
                                             
    # calculating the coverage of 90% CIs
    quan.90.hard <- cbind(quantile(boot.est.hard[,1], c(0.05,0.95)),quantile(boot.est.hard[,2], c(0.05,0.95)))
    
    cover.90.hard.percentile <- cover.90.hard.percentile + c((quan.90.hard[1,1] <= psi.10)&(quan.90.hard[2,1] >= psi.10),(quan.90.hard[1,2] <= psi.11)&(quan.90.hard[2,2] >= psi.11))
    
   
    cover.90.hard.hybrid <- cover.90.hard.hybrid + c((2*main.int.hard[1]-quan.90.hard[2,1] <= psi.10)&(2*main.int.hard[1]-quan.90.hard[1,1] >= psi.10),
                                             (2*main.int.hard[2]-quan.90.hard[2,2] <= psi.11)&(2*main.int.hard[2]-quan.90.hard[1,2] >= psi.11))
                                                                                           
}

# Variance
est.squared.hard <- est.squared.hard/simsize - (bias.hard/simsize)*(bias.hard/simsize)

# Bias
bias.hard <- bias.hard/simsize - c(psi.10, psi.11)

# MSE
MSE <- bias.hard*bias.hard + est.squared.hard

# Rejection rates of nominal 95# CIs using percentile bootstrap
cover.95.hard.percentile <- cover.95.hard.percentile/simsize

# Rejection rates of nominal 95# CIs using hybrid bootstrap
cover.95.hard.hybrid <- cover.95.hard.hybrid/simsize

# Rejection rates of nominal 90# CIs using percentile bootstrap
cover.90.hard.percentile <- cover.90.hard.percentile/simsize

# Rejection rates of nominal 90# CIs using hybrid bootstrap
cover.90.hard.hybrid <- cover.90.hard.hybrid/simsize

final.res <- NULL
final.res$gamma <- gamma
final.res$delta <- delta
final.res$Bias <- bias.hard
final.res$Variance <- est.squared.hard
final.res$MSE <- MSE
final.res$PB95 <- cover.95.hard.percentile
final.res$HB95 <- cover.95.hard.hybrid
final.res$PB90 <- cover.90.hard.percentile
final.res$HB90 <- cover.90.hard.hybrid
final.res$mchosen <- c(mean(m.chosen), median(m.chosen))