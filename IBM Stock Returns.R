spxibm <- as.matrix(read.table("http://www.burns-stat.com/pages/Tutor/spx_ibm.txt",header=TRUE, sep='\t', row.names=1))

spxret <- spxibm[, 'spx']
ibmret <- spxibm[, 'ibm']

spx.boot.sum <- numeric(1000000)

for(i in 1:1000000) {
  this.samp <- spxret[ sample(251, 251, replace = T)]
  spx.boot.sum[i] <- sum(this.samp)
}

plot(density(spx.boot.sum), lwd=3, col="steelblue")
abline(v=sum(spxret), lwd=3, col='gold')

coef(lm(ibmret ~ spxret))