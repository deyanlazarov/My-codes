qvarcomp<-function (datax=x, datay=y,nq){ #predefine X and Y; define nq = number of quantiles
  x1<-sort(datax)
  y1<-sort(datay)
  mx1<-mean(x1)
  my1<-mean(y1)
  probs = seq(0, 1, (1/nq+0.01))
  qx<-quantile(x1, probs = probs)
  qy<-quantile(y1, probs = probs)
  set.seed(1234)
  xn<-rnorm(length(x1),mean(x1),sd(x1))
  xn<-sort(xn)
  yn<-rnorm(length(y1),mean(y1),sd(y1))
  yn<-sort(yn)
  mxn<-mean(xn)
  myn<-mean(yn)
  qxn<-quantile(xn, probs = probs)
  qyn<-quantile(yn, probs = probs)
  
  
  s<-rep(0,length(x1))
  d<-rep(0,length(y1))
  sn<-rep(0,length(xn))
  dn<-rep(0,length(yn))
  
  ###X
  for (m in 1:length(x1)){
    s[m]<-sum((x1[m]-mx1)^2)
  }
  
  ssx<-rep(0,length(probs))
  for (i in 2:length(probs)) {
    j<-1
    while (qx[i]>= x1[j]){
      ssx[i]<-ssx[i]+s[j]
      j=j+1
    }
  }
  ssx=ssx/length(x1)
  ssx1=ssx/max(ssx)
  
  ###Y
  for (k in 1:length(y1)){
    d[k]<-sum((y1[k]-my1)^2)  
  }
  
  ssy<-rep(0,length(probs))
  for (f in 2:length(probs)) {
    r<-1
    while (qy[f]>=y1[r]){
      ssy[f]<-ssy[f]+d[r]
      r=r+1
    }
  }
  ssy=ssy/length(y1)
  ssy1=ssy/max(ssy)
  
  ###Xnorm
  for (m1 in 1:length(xn)){
    sn[m1]<-sum((xn[m1]-mxn)^2)
  }
  
  ssxn<-rep(0,length(probs))
  for (i1 in 2:length(probs)) {
    j1<-1
    while (qxn[i1]>= xn[j1]){
      ssxn[i1]<-ssxn[i1]+sn[j1]
      j1=j1+1
    }
  }
  ssxn=ssxn/length(xn)
  ssxn1=ssxn/max(ssxn)
  
  ###Ynorm
  for (k1 in 1:length(yn)){
    dn[k1]<-sum((yn[k1]-myn)^2)  
  }
  
  ssyn<-rep(0,length(probs))
  for (f1 in 2:length(probs)) {
    r1<-1
    while (qyn[f1]>=yn[r1]){
      ssyn[f1]<-ssyn[f1]+dn[r1]
      r1=r1+1
    }
  }
  ssyn=ssyn/length(yn)
  ssyn1=ssyn/max(ssyn)
  
  
  ####Plots
  ox<-probs
  par(mfrow = c(2,2))
  plot(ox,ssy,pch = ".",main="X~Y comparison", xlab="quantiles", ylab="Cumultive VAR")
  lines(ox,ssx,col="blue")
  lines(ox,ssy,col="red")
  legend("bottomright", c("x", "y"), col = c("blue","red"),bty="n", lty = 1:1,
         pch = ".", ncol = 2, cex = 0.8)
  
  plot(ox,ssx1,pch = ".",main="X~Y comparison (%VAR)",xlab="quantiles",ylab="% Cumultive VAR")
  lines(ox,ssx1,col="blue")
  lines(ox,ssy1,col="red")
  legend("bottomright", c("x", "y"), col = c("blue","red"),bty="n", lty = 1:1,
         pch = ".", ncol = 2, cex = 0.8)
  
  plot(ox,ssx1,pch = ".",main="X~Xnorm comparison",xlab="quantiles", ylab="% Cumultive VAR")
  lines(ox,ssx1,col="blue")
  lines(ox,ssxn1,col="green3")
  legend("bottomright", c("x", "norm(x)"), col = c("blue","green3"),bty="n", lty = 1:1,
         pch = ".", ncol = 2, cex = 0.8)
  
  plot(ox,ssy1,pch = ".",main="Y~Ynorm comparison",xlab="quantiles", ylab="% Cumultive VAR")
  lines(ox,ssy1,col="red")
  lines(ox,ssyn1,col="green3")
  legend("bottomright", c("y", "norm(y)"), col = c("red","green3"),bty="n", lty = 1:1,
         pch = ".", ncol = 2, cex = 0.8)
}