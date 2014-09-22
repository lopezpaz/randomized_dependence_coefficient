library(tikzDevice)

t  <- as.matrix(seq(0,2*pi,0.15))
t  <- as.matrix(runif(100,0,2*pi))
x  <- as.matrix(cos(t))+rnorm(100)*0.1
y  <- as.matrix(sin(t))+rnorm(100)*0.1
u  <- as.matrix(ecdf(x)(x))
v  <- as.matrix(ecdf(y)(y))
wx <- matrix(rnorm(10,0,10),2,5)
wy <- matrix(rnorm(10,0,10),2,5)
f  <- cbind(cos(cbind(u,1)%*%wx),sin(cbind(u,1)%*%wx))
g  <- cbind(cos(cbind(v,2)%*%wy),sin(cbind(v,1)%*%wy))
r  <- cancor(f,g)
fx <- f%*%r$xcoef[,1]
gy <- g%*%r$ycoef[,1]

tikz('original.tex',height=1,width=1,standAlone=TRUE);
  par(mar=rep(0,4),oma=rep(0.1,4),lwd=3);
  plot(x,y,pch=19,cex=0.3, xaxt='n', yaxt='n', ann=FALSE,t="p");
dev.off();

tikz('copula.tex',height=1,width=1,standAlone=TRUE);
  par(mar=rep(0,4),oma=rep(0.1,4),lwd=3);
  plot(ecdf(x)(x),ecdf(y)(y),pch=19,cex=0.3, xaxt='n', yaxt='n', ann=FALSE,t="p");
dev.off();

tikz('lines.tex',height=1,width=1,standAlone=TRUE);
  par(mar=rep(0,4),oma=rep(0.1,4),lwd=3);
  plot(fx,gy,pch=19,cex=0.3, xaxt='n', yaxt='n', ann=FALSE, t="p");
dev.off();

tikz('fx.tex',height=1,width=1,standAlone=TRUE);
  par(mar=rep(0,4),oma=rep(0.1,4),lwd=3);
  plot(u,fx,pch=19,cex=0.3, xaxt='n', yaxt='n', ann=FALSE, t="p");
dev.off();

tikz('fy.tex',height=1,width=1,standAlone=TRUE);
  par(mar=rep(0,4),oma=rep(0.1,4),lwd=3);
  plot(v,gy,pch=19,cex=0.3, xaxt='n', yaxt='n', ann=FALSE, t="p");
dev.off();

tikz('rfx.tex',height=1,width=1,standAlone=TRUE)
  par(mfrow=c(5,1),mar=rep(0,4))
  plot(u,f[,1],pch=19,cex=.5,yaxt='n',xaxt='n',ann=FALSE,frame.plot=FALSE,col="#97080E",t="p")
  plot(u,f[,2],pch=19,cex=.5,yaxt='n',xaxt='n',ann=FALSE,frame.plot=FALSE,col="#DA4B0F",t="p")
  plot(u,f[,3],pch=19,cex=.5,yaxt='n',xaxt='n',ann=FALSE,frame.plot=FALSE,col="#E9B104",t="p")
  plot(u,f[,4],pch=19,cex=.5,yaxt='n',xaxt='n',ann=FALSE,frame.plot=FALSE,col="#488C13",t="p")
  plot(u,f[,5],pch=19,cex=.5,yaxt='n',xaxt='n',ann=FALSE,frame.plot=FALSE,col="#1B55C0",t="p")
dev.off()

tikz('rfy.tex',height=1,width=1,standAlone=TRUE)
  par(mfrow=c(5,1),mar=rep(0,4))
  plot(v,g[,1],pch=19,cex=.5,yaxt='n',xaxt='n',ann=FALSE,frame.plot=FALSE,col="#97080E",t="p")
  plot(v,g[,2],pch=19,cex=.5,yaxt='n',xaxt='n',ann=FALSE,frame.plot=FALSE,col="#DA4B0F",t="p")
  plot(v,g[,3],pch=19,cex=.5,yaxt='n',xaxt='n',ann=FALSE,frame.plot=FALSE,col="#E9B104",t="p")
  plot(v,g[,4],pch=19,cex=.5,yaxt='n',xaxt='n',ann=FALSE,frame.plot=FALSE,col="#488C13",t="p")
  plot(v,g[,5],pch=19,cex=.5,yaxt='n',xaxt='n',ann=FALSE,frame.plot=FALSE,col="#1B55C0",t="p")
dev.off()

system("pdflatex original.tex")
system("pdflatex copula.tex")
system("pdflatex lines.tex")
system("pdflatex fx.tex")
system("pdflatex fy.tex")
system("pdflatex rfx.tex")
system("pdflatex rfy.tex")
system("pdflatex pipeline.tex")

system("rm original.tex")
system("rm copula.tex")
system("rm lines.tex")
system("rm fx.tex")
system("rm fy.tex")
system("rm rfx.tex")
system("rm rfy.tex")
system("rm original.pdf")
system("rm copula.pdf")
system("rm lines.pdf")
system("rm fx.pdf")
system("rm fy.pdf")
system("rm rfx.pdf")
system("rm rfy.pdf")

system("rm *log *aux")

print(cor(fx,gy))
