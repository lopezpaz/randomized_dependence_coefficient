source("gentype.r")

n_noise   <- 30
n         <- 500
k1        <- 1
k2        <- 1

power.ace    = power_ace    = array(NA, c(n_types,n_noise))
power.cor    = power_cor    = array(NA, c(n_types,n_noise))
power.dcor   = power_dcor   = array(NA, c(n_types,n_noise))
power.mine   = power_mine   = array(NA, c(n_types,n_noise))
power.hsic   = power_hsic   = array(NA, c(n_types,n_noise))
power.hsiccop = power_hsiccop = array(NA, c(n_types,n_noise))
power.kcca   = power_kcca   = array(NA, c(n_types,n_noise))
power.rdc    = power_rdc    = array(NA, c(n_types,n_noise))

for(ttt in 1:n_types){
  for(nnn in 1:n_noise){
    fname <- paste("result", k1, k2, n, ttt, nnn, sep="_")
    print(paste("Collecting", fname, "..."))
    if(file.exists(fname) == TRUE) load(fname)
    power_ace    [ttt,nnn] = power.ace    [ttt,nnn]
    power_cor    [ttt,nnn] = power.cor    [ttt,nnn]
    power_dcor   [ttt,nnn] = power.dcor   [ttt,nnn]
    power_mine   [ttt,nnn] = power.mine   [ttt,nnn]
    power_hsic   [ttt,nnn] = power.hsic   [ttt,nnn]
    power_hsiccop [ttt,nnn] = power.hsiccop [ttt,nnn]
    power_kcca   [ttt,nnn] = power.kcca   [ttt,nnn]
    power_rdc    [ttt,nnn] = power.rdc    [ttt,nnn]
  }
}

power.ace    = power_ace   
power.cor    = power_cor   
power.dcor   = power_dcor  
power.mine   = power_mine  
power.hsic   = power_hsic  
power.hsiccop = power_hsiccop
power.kcca   = power_kcca  
power.rdc    = power_rdc

load('results_synthetic')

power.rdc  <- power_rdc

#save(
# power.ace   ,
# power.cor   ,
# power.dcor  ,
# power.mine  ,
# power.hsic  ,
# power.hsiccop,
# power.kcca  ,
# power.rdc   ,
# file = "new_baseline")

library("tikzDevice")

tikz("output.tex", standAlone=TRUE, width=7, height=5)

par(mfrow = c(4,2),lwd=1,mar=rep(0,4),oma=c(4,4,0,0),ps=12)


xvals <- c(1:30)/30*100

for(typ in c(1,2,3,5,7,8,9,10)) {
#for(typ in 1:n_types) {
  if(typ == 1) plot  (xvals, power.cor[typ,], pch = 1, col = 1, type = 'b', ylim = c(0,1.1), xaxt='n')
  if(typ == 2) plot  (xvals, power.cor[typ,], pch = 1, col = 1, type = 'b', ylim = c(0,1.1), yaxt='n', xaxt='n')
  if(typ == 3) plot  (xvals, power.cor[typ,], pch = 1, col = 1, type = 'b', ylim = c(0,1.1), xaxt='n')
  if(typ == 5) plot  (xvals, power.cor[typ,], pch = 1, col = 1, type = 'b', ylim = c(0,1.1), yaxt='n', xaxt='n')
  if(typ == 7) plot  (xvals, power.cor[typ,], pch = 1, col = 1, type = 'b', ylim = c(0,1.1), xaxt='n')
  if(typ == 8) plot  (xvals, power.cor[typ,], pch = 1, col = 1, type = 'b', ylim = c(0,1.1), yaxt='n', xaxt='n')
  if(typ == 9) plot  (xvals, power.cor[typ,], pch = 1, col = 1, type = 'b', ylim = c(0,1.1))
  if(typ ==10) plot  (xvals, power.cor[typ,], pch = 1, col = 1, type = 'b', ylim = c(0,1.1), yaxt='n')
  points(xvals,power.dcor  [typ,], pch = 2, col = 2, type = 'b')
  points(xvals,power.mine  [typ,], pch = 3, col = 3, type = 'b')
  points(xvals,power.ace   [typ,], pch = 8, col = 8, type = 'b')
  points(xvals,power.hsic  [typ,], pch = 5, col = 5, type = 'b')
  points(xvals,power.hsiccop[typ,], pch = 6, col = 6, type = 'b')
  points(xvals,power.rdc   [typ,], pch = 4, col = 4, lwd=3,type = 'b')

  if(typ==5)
    legend("topright",c("cor","dCor","MIC", "ACE", "MMD", "CMMD", "RDC"), pch =
    c(1,2,3,8,5,6,4), col = c(1,2,3,8,5,6,4))

  rect(1, .02, 15, .5, col="white")
  xy <- gentype(typ,100,0,0,1)
  xy$x <- (xy$x-min(xy$x))/(max(xy$x)-min(xy$x))*12+2
  xy$y <- (xy$y-min(xy$y))/(max(xy$y)-min(xy$y))*0.42+.05
  ooo  <- order(xy$x)
  points(xy$x[ooo], xy$y[ooo],pch=19,cex=0.3)
}

title(xlab="Noise Level", ylab="Power", outer=TRUE)

dev.off()

system("pdflatex output.tex")

