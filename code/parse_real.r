library("tikzDevice")
tikz("real.tex",height=4.5,width=7,standAlone=TRUE)

add.error.bars <- function(X,Y,SE,w,col=1){
  X0 = X; Y0 = (Y-SE); X1 =X; Y1 = (Y+SE);
  arrows(X0, Y0, X1, Y1, code=3,angle=90,length=w,col=col);
}

par(mfrow=c(3,4), mar=rep(2,4),oma=c(2,2,0,0),mgp=c(1,1,0),lwd=2,ps=12)

for(fname in 
  c("results_abalone",
    "results_autompg",
    "results_breast",
    "results_calcensus",
    "results_concrete",
    "results_cpuact",
    "results_housing",
    "results_machine",
    "results_parkinson",
    "results_sarcos",
    "results_stock",
    "results_whitewine")) {
  x <- read.table(fname)

  dco   <- NULL
  hsi   <- NULL
  mmd   <- NULL
  rdc   <- NULL
  s_dco <- NULL
  s_hsi <- NULL
  s_mmd <- NULL
  s_rdc <- NULL
  
  for(f in unique(x[,2])) {
    dco   <- c(dco,mean(x[x[,2]==f,3]))
    hsi   <- c(hsi,mean(x[x[,2]==f,4]))
    mmd   <- c(mmd,mean(x[x[,2]==f,5]))
    rdc   <- c(rdc,mean(x[x[,2]==f,6]))
    s_dco <- c(s_dco,sd(x[x[,2]==f,3]))
    s_hsi <- c(s_hsi,sd(x[x[,2]==f,4]))
    s_mmd <- c(s_mmd,sd(x[x[,2]==f,5]))
    s_rdc <- c(s_rdc,sd(x[x[,2]==f,6]))
  }

  print(s_dco)
  
  title <- strsplit(as.character(x[1,1]), "_")[[1]][2]
  title <- strsplit(title, "\\.")[[1]][1]
 
  plot (1:length(dco), dco, lwd=1, t="b", col=1,pch=1,
  ylim=c(min(dco,hsi,mmd,rdc), max(dco,hsi,mmd,rdc)),
  main=title,xlab="",ylab="")
  lines(1:length(dco), hsi, lwd=1, t="b", col=2,pch=2)
  lines(1:length(dco), mmd, lwd=1, t="b", col=3,pch=3)
  lines(1:length(dco), rdc, lwd=2, t="b", col=4,pch=4)

  if(fname == "results_calcensus")
    legend("topright",c("dCor", "MMD", "CMMD", "RDC"), pch = 1:4, col = 1:4)
}

title(xlab="Number of features", ylab="Gaussian Process NMSE", outer=TRUE)
dev.off()
system("pdflatex real.tex")
system("cp real.pdf ~/Projects/rdc/figures")

