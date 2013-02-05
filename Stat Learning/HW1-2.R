HW <- read.table('HW2.txt')
HW <- HW[, 2:3]

library(mnormt)
oracle <- function(z){
	var.mat <- diag(2)/5
	p <- c(0,0)
	for (i in 1:10){
		p[1] = p[1] + dmnorm(z, HW[i, ], var.mat)/10
		p[2] = p[2] + dmnorm(z, HW[10+i,], var.mat)/10
	}
	return(p[1]-p[2])
}

x <- seq(min(HW[,1]), max(HW[,1]), 0.05)
y <- seq(min(HW[,2]), max(HW[,2]), 0.05)

z <- matrix(0, length(x), length(y))

for (i in 1:length(x)){
	for (j in 1: length(y)){
		z[i, j] = oracle(c(x[i], y[j]))
	}
	if (i%%10 == 0){
		print(i)
	}
}

write.table(z, file =  'HW2_contour.csv', sep = ',', row.names = F)

contour(x, y, z, levels=0)