library(ElemStatLearn); data(ozone, package="ElemStatLearn")

## Order by the ozone levels
  ozone <- ozone[order(ozone$ozone),]
  head(ozone)

## Do a bagged LOESS
  ll <- matrix(NA, nrow=100, ncol=155)
  for(i in 1:100){
    ss <- sample(1:dim(ozone)[1],replace=T)
    ozone0 <- ozone[ss,]; ozone0 <- ozone0[order(ozone0$ozone),]
    loess0 <- loess(temperature ~ ozone, data=ozone0, span=0.2)
    ll[i,] <- predict(loess0,newdata=data.frame(ozone=1:155))
  }
  
## Graph the LOESS
  plot(ozone$ozone,ozone$temperature,pch=19,cex=0.5)
  for(i in 1:100){lines(1:155,ll[i,],col='grey',lwd=2)}
  lines(1:155,apply(ll,2,mean),col='red',lwd=2)