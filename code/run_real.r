d <- c("/is/ei/dlopez/datasets/regression_abalone.txt",
"/is/ei/dlopez/datasets/regression_ailerons.txt",
"/is/ei/dlopez/datasets/regression_automobile.txt",
"/is/ei/dlopez/datasets/regression_autompg.txt",
"/is/ei/dlopez/datasets/regression_bank32.txt",
"/is/ei/dlopez/datasets/regression_breast.txt",
"/is/ei/dlopez/datasets/regression_calhousing.txt",
"/is/ei/dlopez/datasets/regression_concrete.txt",
"/is/ei/dlopez/datasets/regression_cpuact.txt",
"/is/ei/dlopez/datasets/regression_crime.txt",
"/is/ei/dlopez/datasets/regression_delevators.txt",
"/is/ei/dlopez/datasets/regression_deltaailerons.txt",
"/is/ei/dlopez/datasets/regression_elevators.txt",
"/is/ei/dlopez/datasets/regression_eunite.txt",
"/is/ei/dlopez/datasets/regression_fat.txt",
"/is/ei/dlopez/datasets/regression_forest.txt",
"/is/ei/dlopez/datasets/regression_frieddelve.txt",
"/is/ei/dlopez/datasets/regression_hardware.txt",
"/is/ei/dlopez/datasets/regression_house16.txt",
"/is/ei/dlopez/datasets/regression_housing.txt",
"/is/ei/dlopez/datasets/regression_insurance.txt",
"/is/ei/dlopez/datasets/regression_kin8.txt",
"/is/ei/dlopez/datasets/regression_machine.txt",
"/is/ei/dlopez/datasets/regression_metabolic.txt",
"/is/ei/dlopez/datasets/regression_mg.txt",
"/is/ei/dlopez/datasets/regression_mvdelve.txt",
"/is/ei/dlopez/datasets/regression_parkinson.txt",
"/is/ei/dlopez/datasets/regression_pol.txt",
"/is/ei/dlopez/datasets/regression_puma32.txt",
"/is/ei/dlopez/datasets/regression_pyrim.txt",
"/is/ei/dlopez/datasets/regression_redwine.txt",
"/is/ei/dlopez/datasets/regression_sarcos.txt",
"/is/ei/dlopez/datasets/regression_sin10xy.txt",
"/is/ei/dlopez/datasets/regression_sine.txt",
"/is/ei/dlopez/datasets/regression_slice.txt",
"/is/ei/dlopez/datasets/regression_slump.txt",
"/is/ei/dlopez/datasets/regression_stock.txt",
"/is/ei/dlopez/datasets/regression_triazines.txt",
"/is/ei/dlopez/datasets/regression_whitewine.txt",
"/is/ei/dlopez/datasets/regression_weather.txt")

source("algorithms.r")

for(di in as.double(commandArgs(TRUE)[1])) {
  title <- strsplit(d[di], "_")[[1]][2]
  title <- strsplit(title, "\\.")[[1]][1]
  x    <- as.matrix(scale(read.table(d[di])))
  y    <- as.matrix(x[,ncol(x)])
  x    <- as.matrix(x[,-ncol(x)])

  for(r in 1:10) {
    i    <- sample(1:nrow(x), floor(0.5*nrow(x)))
    j    <- setdiff(1:nrow(x), i)
    x_tr <- as.matrix(x[i,])
    y_tr <- as.matrix(y[i,])
    x_te <- as.matrix(x[j,])
    y_te <- as.matrix(y[j,])
    maxk <- min(1000,nrow(x_tr))
    Fdco <- NULL 
    Fhsi <- NULL 
    Fmmd <- NULL 
    Frdc <- NULL 
    Edco <- 0
    Ehsi <- 0
    Emmd <- 0
    Erdc <- 0
    k    <- 10
    s    <- 0.2
    
    for(f in 1:min(ncol(x),10)) {
      Rdco <- rep(0,ncol(x))
      Rhsi <- rep(0,ncol(x))
      Rmmd <- rep(0,ncol(x))
      Rrdc <- rep(0,ncol(x))
    
      # DCOR
      for(i in setdiff(1:ncol(x), Fdco)) Rdco[i] <- dcor(x_tr[1:maxk,c(Fdco,i)], y_tr[1:maxk])
      Fdco <- c(Fdco, order(Rdco,decreasing=TRUE)[1])
      Edco <- (mean((predict(gausspr(x_tr[1:maxk,Fdco],y_tr[1:maxk],scaled=NULL),x_te[,Fdco])-y_te)^2))
      
      # HSIC
      for(i in setdiff(1:ncol(x), Fhsi)) Rhsi[i] <- hsic(x_tr[1:maxk,c(Fhsi,i)], y_tr[1:maxk])
      Fhsi <- c(Fhsi, order(Rhsi,decreasing=TRUE)[1])
      Ehsi <- (mean((predict(gausspr(x_tr[1:maxk,Fhsi],y_tr[1:maxk],scaled=NULL),x_te[,Fhsi])-y_te)^2))
  
      # MMDCOP
      for(i in setdiff(1:ncol(x), Fmmd)) Rmmd[i] <- mmdcop(x_tr[1:maxk,c(Fmmd,i)], y_tr[1:maxk])
      Fmmd <- c(Fmmd, order(Rmmd,decreasing=TRUE)[1])
      Emmd <- (mean((predict(gausspr(x_tr[1:maxk,Fmmd],y_tr[1:maxk],scaled=NULL),x_te[,Fmmd])-y_te)^2))
      
      # RDC 
      for(i in setdiff(1:ncol(x), Frdc)) Rrdc[i] <- rdc(x_tr[,c(Frdc,i)], y_tr,k,s)
      Frdc <- c(Frdc, order(Rrdc,decreasing=TRUE)[1])
      Erdc <- (mean((predict(gausspr(x_tr[1:maxk,Frdc],y_tr[1:maxk],scaled=NULL),x_te[,Frdc])-y_te)^2))
    
      print(paste(d[di],f, Edco, Ehsi, Emmd, Erdc))
      write(paste(d[di],f, Edco, Ehsi, Emmd, Erdc), append=TRUE, file=paste("results_",title,sep=""))
    }
  }
}

