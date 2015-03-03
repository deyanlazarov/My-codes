clt<-function(n,mu,s,k){#n=sample size;mu=population mean;s=population sd,k=number of repitition of the sampling
  xbar=rep(0,k)
  for (i in 1:k) {xbar[i]=mean(rnorm(n,mean=mu,sd=s))}
  a<-round(mean(xbar[i]),3)
    par(mfrow = c(2,1))
        hist(xbar,prob=TRUE,breaks=12,xlim=c(mu-30,mu+30),ylim=c(0,0.2), col="lightcyan",main="Normal Population. Histogram of Xbar & ND")
        legend(mu+20,0.15,a, bty ="n")
        legend(mu+15,0.20, c("Mean Xbar ="), bty="n")
        curve(dnorm(x,mean=mu,sd=s/sqrt(n)),mu-4*s/sqrt(n),mu+4*s/sqrt(n),add=TRUE,lwd=2,col="red")
        curve(dnorm(x,mu,sd=s),mu-4*s,mu+4*s,add=TRUE,lwd=2,col="green4")
    
        plot(density(xbar),ylim=c(0,0.2),main = "Real Density Xbar & ND")
        curve(dnorm(x,mean=mu,sd=s/sqrt(n)),mu-4*s/sqrt(n),mu+4*s/sqrt(n), add=TRUE,lwd=2,col="red")
  library(psych)
  describe(xbar)
}
