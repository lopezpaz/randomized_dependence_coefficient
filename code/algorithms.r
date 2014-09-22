library(acepack)
library(minerva)
library(kernlab)
library(energy)

computeKernelMatrix <- function(sample) {
	n        <- nrow(sample)
	Q        <- matrix(apply(sample^2, 1, sum), n, n)
	distance <- Q + t(Q) - 2 * sample %*% t(sample)
	exp(-sigest(sample,scale=NULL)[2]*distance)
}

hsic <- function(sampleX, sampleY) {
	N  <- nrow(as.matrix(sampleX))
	K  <- computeKernelMatrix(as.matrix(sampleX))
	L  <- computeKernelMatrix(as.matrix(sampleY))
	KH <- K - 1 / N * matrix(apply(K, 2, sum), N, N)
	LH <- L - 1 / N * matrix(apply(L, 2, sum), N, N)
	1 / N * sum(sum(KH * t(LH)))
}

hsiccop <- function(sampleX, sampleY) {
  sampleX <- apply(as.matrix(sampleX),2,function(u) ecdf(u)(u))
  sampleY <- apply(as.matrix(sampleY),2,function(u) ecdf(u)(u))
  hsic(sampleX,sampleY)
}

rdc <- function(x,y,k=20,s=1/6,f=sin) {
  x <- cbind(apply(as.matrix(x),2,function(u)rank(u)/length(u)),1)
  y <- cbind(apply(as.matrix(y),2,function(u)rank(u)/length(u)),1)
  x <- s/ncol(x)*x%*%matrix(rnorm(ncol(x)*k),ncol(x))
  y <- s/ncol(y)*y%*%matrix(rnorm(ncol(y)*k),ncol(y))
  cancor(cbind(f(x),1),cbind(f(y),1))$cor[1]
}
