#Homework 9
#with replacement for bootstrap,because you would be limited in number of permutations if you dont replace
#find CI to see if data you obtained is extreme or not -Chakraborti 


#example 4.9.1
#book function
percentciboot <- function(x, b, alpha){
  set.seed(12345)
  theta <- mean(x); thetastar <- rep(0, b); n <- length(x)
  for(i in 1:b){
    xstar <- sample(x, n, replace=T)
    thetastar[i] <- mean(xstar)
  }
    thetastar <- sort(thetastar)
    pick <- round((alpha/2)*(b+1))
    lower <- thetastar[pick]; upper <- thetastar[b-pick+1]
    list(theta=theta, lower=lower, upper=upper)
  hist(thetastar)
  cat("CI:[", lower, ",", upper, "], and mean:", mean(thetastar))
}

#TA example function
bootstrap.f <- function(X, sim=1000, alpha=0.1){
  set.seed(12345)
  x.bar.vec <- unlist(lapply(1:sim, function(x){
    Y <- sample(X, 20, replace=T)
    mean(Y)
  }))
  quantile(x.bar.vec, c(alpha/2, 1-alpha/2))
}
bootstrap.f(sample, sim=3000)

#my example function
bootstrap.my <- function(X, sim=1000, alpha=0.1){
  set.seed(12345)
  x.bar.vec <- sapply(1:sim, function(x){
    Y <- sample(X, 20, replace=T)
    mean(Y)
  })
  quantile(x.bar.vec, c(alpha/2, 1-alpha/2))
}
bootstrap.my(sample, sim=3000)



#generate sample, then bootstrap
set.seed(12345)
n <- 20
sample1 <- rgamma(n, shape=1, scale=100)
mean(sample1)
hist(sample1)
percentciboot(sample1, 3000, 0.1)




#using the book's original sample
sample <- c(131.7, 182.7, 73.3, 10.7, 150.4, 42.3, 22.2, 17.9, 264, 154.4, 4.3, 265.6, 61.9, 10.8, 48.8, 22.5, 8.8, 150.6, 103, 85.9)
mean(sample)
hist(sample)
percentciboot(sample, 3000, 0.1)



u=seq(0,3.09,by=0.01)
p=pnorm(u)
m=matrix(p,ncol=10,byrow=TRUE)

options(digits=4)
m


