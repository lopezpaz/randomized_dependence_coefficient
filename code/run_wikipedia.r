library(tikzDevice)
library(mvtnorm)

source("algorithms.r")

one_experiment <- function(xy, xlim = c(-4, 4), ylim = c(-4, 4), eps = 1e-15) {
  if (sd(xy[,2]) < eps) {
    title = ""
  } else {
    res_ace     <- ace(xy[,1],xy[,2])
    res_ace     <- sprintf("%1.1f",round(cor(res_ace$tx,res_ace$ty),1))
    res_rdc     <- sprintf("%1.1f",round(rdc(xy[,1], xy[,2]), 1))
    res_dcor    <- sprintf("%1.1f",round(dcor(as.matrix(xy[,1]), as.matrix(xy[,2])), 1))
    res_mine    <- sprintf("%1.1f",round(mine(xy[,1],xy[,2])$MIC,1))
    res_rho     <- sprintf("%1.1f",round(cor(xy[,1],xy[,2], method="pearson"),1))
    res_spear   <- sprintf("%1.1f",round(cor(xy[,1],xy[,2], method="spear"),1))
    res_kendall <- sprintf("%1.1f",round(cor(xy[,1],xy[,2], method="kendall"),1))

    title = paste(
      res_rdc,
      res_ace,
      res_dcor,
      res_mine,
      "\n",
      res_rho,
      res_spear,
      res_kendall)

    title <- paste( 
    "\\textmd { \\begin{tabular}{|c|c|c|c|} \\hline", res_rdc, "&", res_ace, "&",
    res_dcor, "&", res_mine, "\\\\ \\hline  &", res_rho, "&", res_spear, "&",
    res_kendall, "\\\\ \\hline \\end{tabular}}"
    )
  }

  plot(xy[1:200,], main = title, xlab = "", ylab = "",
       col = "darkblue", pch = 16, 
       xaxt = "n", yaxt = "n", bty = "n",
       xlim = xlim, ylim = ylim)
}

MvNormal <- function(n = 10000, cor = 0.8) {
  for (i in cor) {
    sd = matrix(c(1, i, i, 1), ncol = 2)
      x = rmvnorm(n, c(0, 0), sd)
      one_experiment(x)
  }
}

rotation <- function(t, X) return(X %*% matrix(c(cos(t), sin(t), -sin(t), cos(t)), ncol = 2))

RotNormal <- function(n = 1000, t = pi/2) {
  sd = matrix(c(1, 1, 1, 1), ncol = 2)
  x  = rmvnorm(n, c(0, 0), sd)
  for (i in t) one_experiment(rotation(i, x))
}

Others <- function(n = 1000, spread = 1) {
  x = runif(n, -1, 1) #same image, not a time series (no serial dependence)
  y = 4 * (x^2 - 1/2)^2 + runif(n, -1, 1) /3 * spread
  one_experiment(cbind(x,y), xlim = c(-1, 1), ylim = c(-1/3, 1+1/3))
  
  y   = runif(n, -1, 1)
  xy  = rotation(-pi/8, cbind(x,y))
  lim = sqrt(2+sqrt(2)) / sqrt(2)
  one_experiment(cbind(xy[,1], xy[,2]*spread), xlim = c(-lim, lim), ylim = c(-lim, lim))
  
  xy = rotation(-pi/8, xy)
  one_experiment(cbind(xy[,1], xy[,2]*spread), xlim = c(-sqrt(2), sqrt(2)), ylim = c(-sqrt(2), sqrt(2)))
  
  y = 2*x^2 + runif(n, -1, 1) * spread
  one_experiment(cbind(x,y), xlim = c(-1, 1), ylim = c(-1, 3))
  
  y = (x^2 + runif(n, 0, 1/2) * spread) * sample(seq(-1, 1, 2), n, replace = TRUE)
  one_experiment(cbind(x,y), xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))
  
  y = cos(x*pi) + rnorm(n, 0, 1/8) * spread
  x = sin(x*pi) + rnorm(n, 0, 1/8) * spread
  one_experiment(cbind(x,y), xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))
  
  xy1 = rmvnorm(n/4, c( 3,  3), diag(2)*spread)
  xy2 = rmvnorm(n/4, c(-3,  3), diag(2)*spread)
  xy3 = rmvnorm(n/4, c(-3, -3), diag(2)*spread)
  xy4 = rmvnorm(n/4, c( 3, -3), diag(2)*spread)
  one_experiment(rbind(xy1, xy2, xy3, xy4)[sample(n),], xlim = c(-3-4, 3+4), ylim = c(-3-4, 3+4))
}

n <- 800
tikz('test.tex',standAlone=TRUE,width=9,height=4)
par(mfrow = c(2, 7), oma = c(0,0,0,0), mar=c(0,2,2.5,2),ps=10)
MvNormal(n, c(1.0, 0.8, 0.4, 0.0, -0.4, -0.8, -1.0));
#RotNormal(200, c(0, pi/12, pi/6, pi/4, pi/2-pi/6, pi/2-pi/12, pi/2));
#Others(800)
Others(n, 0.3)
dev.off()

