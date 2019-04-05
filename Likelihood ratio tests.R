#6.3.17
x <- c(27, 13, 21, 24, 22, 14, 17, 26, 14, 22,
       21, 24, 19, 25, 15, 25, 23, 16, 20, 19)
theta.0 <- 23

wald.test <- function(x, theta){
  theta.hat = mean(x)
  FI.hat <- 1 / theta.hat
  n <- length(x)
  wald.stat <<- (sqrt(n * FI.hat) * (theta.hat - theta.0))^2
  return(wald.stat)
}

wald.test(x, theta.0)

p.val <- 1 - pchisq(wald.stat, 1)
p.val
