ttt <- as.integer(commandArgs(TRUE)[1])
nnn <- as.integer(commandArgs(TRUE)[2])

source("algorithms.r")

print(paste("Experiment of type", ttt, "with noise level", nnn))

nsim      = 500
nsim      = 500
n         = 500
num.noise = 30
noise     = 3
val.cor   = val.dcor   = val.mine   = val.ace    = val.mmdcop   = val.hsic   = val.rdc   = rep(NA,nsim)
val.cor2  = val.dcor2  = val.mine2  = val.ace2   = val.mmdcop2  = val.hsic2  = val.rdc2  = rep(NA,nsim)
power.cor = power.dcor = power.mine = power.ace  = power.mmdcop = power.hsic = power.rdc = array(NA, c(n_types,num.noise))

set.seed(1)

k1 = 1
k2 = 1

for(typ in ttt) {
  for(l in nnn) {
    for(ii in 1:nsim) {
      xy = gentype(typ, n, l, noise, num.noise)
      x  = runif(n)
      y  = xy$y

      val.rdc [ii]   = rdc(x,y)
      val.dcor[ii]   = dcor(x,y)
      val.ace [ii]   = ace(x,y)$rsq
      val.cor [ii]   = (cor(x,y))^2
      val.mine[ii]   = mine(x,y)$MIC
      val.hsic[ii]   = hsic(x,y)
      val.mmdcop[ii] = mmdcop(x,y)
    }

    cut.rdc    = quantile(val.rdc ,.95)
    cut.dcor   = quantile(val.dcor,.95)
    cut.ace    = quantile(val.ace, .95)
    cut.cor    = quantile(val.cor, .95)
    cut.mine   = quantile(val.mine,.95)
    cut.hsic   = quantile(val.hsic ,.95)
    cut.mmdcop = quantile(val.mmdcop,.95)

    for(ii in 1:nsim){
      xy = gentype(typ, n, l, noise, num.noise)
      x  = xy$x
      y  = xy$y

      val.rdc2 [ii]   = rdc(x,y)
      val.dcor2[ii]   = dcor(x,y)
      val.ace2 [ii]   = ace(x,y)$rsq
      val.cor2 [ii]   = cor(x,y)^2
      val.mine2[ii]   = mine(x,y)$MIC
      val.hsic2[ii]   = hsic(x,y)
      val.mmdcop2[ii] = mmdcop(x,y)
    }

    power.rdc [typ,l]   = sum(val.rdc2  > cut.rdc) /nsim
    power.dcor[typ,l]   = sum(val.dcor2 > cut.dcor)/nsim
    power.ace [typ,l]   = sum(val.ace2  > cut.ace) /nsim
    power.cor [typ,l]   = sum(val.cor2  > cut.cor) /nsim
    power.mine[typ,l]   = sum(val.mine2 > cut.mine)/nsim
    power.hsic[typ,l]   = sum(val.hsic2 > cut.hsic) /nsim
    power.mmdcop[typ,l] = sum(val.mmdcop2 > cut.mmdcop) /nsim

    print(paste("Type:", typ, "Noise:", l,
          "ACE:", power.ace [typ,l],
          "COR:", power.cor [typ,l],
          "DCO:", power.dcor[typ,l],
          "MIC:", power.mine[typ,l],
          "HSI:", power.hsic[typ,l],
          "MMD:", power.mmdcop[typ,l],
          "RDC:", power.rdc [typ,l]))
  }
}

save(power.ace, power.cor, power.dcor, power.mine, power.hsic, power.mmdcop, power.rdc,
     file = paste("result", k1, k2, n, ttt, nnn, sep="_"))

