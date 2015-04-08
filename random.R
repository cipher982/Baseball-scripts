ppois(10,2)
ppois(1,1)
ppois(2,1)
ppois(3,1)
ppois(12,2)
rpois?
require("yacas")

p <- function(5+1)
  
  ((21 ** 12) * (2.7183 ** -21)) / factorial(12)

hilbert <- function(n) {
  i <- 1:n
  1 / outer(i - 1, i, "+")
}

x <- hilbert(1000)
system.time(svd(x))


set.seed(10)
x <- rep(0:1, each = 5)
e <- rnorm(10, 0, 20)
y <- 0.5 + 2 * x + e

seq(111,10)
rep(1,10)

iris <- iris
yesorno <- character(nrow(iris))

for (i in 1:nrow(iris)){
  
  if (iris$Sepal.Length[i] > 5) yesorno[i] <- "greater than 5"
  else yesorno[i] <- "Less than 5"

}
iris <- cbind(iris,yesorno)