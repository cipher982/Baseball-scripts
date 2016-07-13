# This function takes two ticker symbols as arguements and does some work to it


# import daily data and tidy it up 
StockCorrelation <- function(stock1, stock2)
  
  {
  require(TTR)
  stock1_lev <- drop(as.matrix(getYahooData(stock1, 20000101, 20160205)[,"Close"]))
  stock2_lev <- drop(as.matrix(getYahooData(stock2, 20000101, 20160205)[,"Close"]))
  
  # if this is not true, then take evasive action
  all.equal(names(stock1_lev), names(stock2_lev)) 
  comdate <- intersect(names(stock1_lev), names(stock2_lev))
  setdiff(names(stock2_lev), names(stock1_lev))
  stock2_lev <- stock2_lev[comdate]
  stock1_lev <- stock1_lev[comdate]
  # end if
  
  stock1_ret <- diff(log(stock2_lev))
  stock2_ret <- diff(log(stock1_lev))
  
  # now make monthly data
  # 'to.monthly' from 'xts' package on which 'TTR' depends
  
  # head(x, -1) drops last (partial) month
  stock1.monlev <- head(drop(as.matrix(to.monthly(stock1_lev)[,"stock1_lev.Close"])), -1)
  stock2.monlev <- head(drop(as.matrix(to.monthly(stock2_lev)[,"stock2_lev.Close"])), -1)
  stock2.monret <- diff(log(stock2.monlev))
  stock1.monret <- diff(log(stock1.monlev))
  
  # basically Figure 1
  plot(stock1.monret, stock2.monret)
  points(tail(stock1.monret, 60), tail(stock2.monret, 60), col="blue", lwd=2)
  
  # get final 60 months
  stock1.m60 <- tail(stock1.monret, 60)
  stock2.m60 <- tail(stock2.monret, 60)
  
  # regression and bootstrap with monthly data
  summary(lm(stock2.m60 ~ stock1.m60))
  coef(lm(stock2.m60 ~ stock1.m60))
  btm <- numeric(10000)
  for(i in 1:10000){
    samp60 <- sample(60, 60, replace=TRUE)
    btm[i] <- coef(lm(stock2.m60[samp60] ~ stock1.m60[samp60]))[2]
  }
  plot(density(btm)) # basically Figure 2
  mean(btm > 1)
  mean(btm < 0.5)
  
  # regression and bootstrap on daily data
  stock1.d250 <- tail(stock1_ret, 250)
  stock2.d250 <- tail(stock2_ret, 250)
  coef(lm(stock2.d250 ~ stock1.d250))
  btd <- numeric(10000)
  for(i in 1:10000) {
    s250 <- sample(250, 250, replace=TRUE)
    btd[i] <- coef(lm(stock2.d250[s250] ~ stock1.d250[s250]))[2]
  }
  plot(density(btd)) # basically Figure 3
  
  # 'garch' function from 'tseries' package
  
  require(tseries)
  
  stock1_gar <- garch(stock1_ret)
  stock2_gar <- garch(stock2_ret)
  
  plot(stock1_gar$fitted[,1], type='l')
  plot(stock2_gar$fitted[,1], type='l')
  plot(stock2_gar$fitted[,1]/ stock1_gar$fitted[,1], type='l')
  
  # put 'pp.timeplot' function into global environment
  source('http://www.portfolioprobe.com/R/blog/pp.timeplot.R')
  
  # need names (that are dates) to use pp.timeplot
  volratio <- stock2_gar$fitted[,1]/ stock1_gar$fitted[,1]
  names(volratio) <- names(stock1_ret)
  pp.timeplot(volratio) # basically Figure 4
  
  # get rolling correlation
  stock12_cor200 <- stock1_ret
  Stock21_cor200[] <- NA
  seq200 <- -199:0
  for(i in 200:length(stock2_ret)) stock21_cor200[i] <- cor(stock2_ret[seq200+i], stock1_ret[seq200+i])
  pp.timeplot(stock21_cor200) # basically Figure 5
  pp.timeplot(stock21_cor200 * volratio) # basically Figure 6

}