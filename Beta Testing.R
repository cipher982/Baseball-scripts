# import daily data and make it right

require(TTR)
spxlev <- drop(as.matrix(getYahooData("^GSPC", 20000101, 20110205)[, "Close"]))
ibmlev <- drop(as.matrix(getYahooData("IBM", 20000101, 20110205)[,"Close"]))

# if this is not true, then take evasive action
all.equal(names(spxlev), names(ibmlev)) 
comdate <- intersect(names(spxlev), names(ibmlev))
setdiff(names(ibmlev), names(spxlev))
ibmlev <- ibmlev[comdate]
spxlev <- spxlev[comdate]
# end if

ibmret <- diff(log(ibmlev))
spxret <- diff(log(spxlev))

# now make monthly data
# 'to.monthly' from 'xts' package on which 'TTR' depends

# head(x, -1) drops last (partial) month
spx.monlev <- head(drop(as.matrix(to.monthly(spxlev)[,"spxlev.Close"])), -1)
ibm.monlev <- head(drop(as.matrix(to.monthly(ibmlev)[,"ibmlev.Close"])), -1)
ibm.monret <- diff(log(ibm.monlev))
spx.monret <- diff(log(spx.monlev))

# basically Figure 1
plot(spx.monret, ibm.monret)
points(tail(spx.monret, 60), tail(ibm.monret, 60), col="blue", lwd=2)

# get final 60 months
spx.m60 <- tail(spx.monret, 60)
ibm.m60 <- tail(ibm.monret, 60)

# regression and bootstrap with monthly data
summary(lm(ibm.m60 ~ spx.m60))
coef(lm(ibm.m60 ~ spx.m60))
btm <- numeric(10000)
for(i in 1:10000){
  samp60 <- sample(60, 60, replace=TRUE)
  btm[i] <- coef(lm(ibm.m60[samp60] ~ spx.m60[samp60]))[2]
}
plot(density(btm)) # basically Figure 2
mean(btm > 1)
mean(btm < 0.5)

# regression and bootstrap on daily data
spx.d250 <- tail(spxret, 250)
ibm.d250 <- tail(ibmret, 250)
coef(lm(ibm.d250 ~ spx.d250))
btd <- numeric(10000)
for(i in 1:10000) {
  s250 <- sample(250, 250, replace=TRUE)
  btd[i] <- coef(lm(ibm.d250[s250] ~ spx.d250[s250]))[2]
}
plot(density(btd)) # basically Figure 3

# 'garch' function from 'tseries' package

require(tseries)

spxgar <- garch(spxret)
ibmgar <- garch(ibmret)

plot(spxgar$fitted[,1], type='l')
plot(ibmgar$fitted[,1], type='l')
plot(ibmgar$fitted[,1]/ spxgar$fitted[,1], type='l')

# put 'pp.timeplot' function into global environment
source('http://www.portfolioprobe.com/R/blog/pp.timeplot.R')

# need names (that are dates) to use pp.timeplot
volratio <- ibmgar$fitted[,1]/ spxgar$fitted[,1]
names(volratio) <- names(spxret)
pp.timeplot(volratio) # basically Figure 4

# get rolling correlation
ibmspxcor200 <- spxret
ibmspxcor200[] <- NA
seq200 <- -199:0
for(i in 200:length(ibmret)) ibmspxcor200[i] <- cor(ibmret[seq200+i], spxret[seq200+i])
pp.timeplot(ibmspxcor200) # basically Figure 5
pp.timeplot(ibmspxcor200 * volratio) # basically Figure 6
