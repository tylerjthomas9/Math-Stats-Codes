#4.8.2
n <- 10000
u <- runif(n)
x <- 1/(1+u)
hist(x)
xbar <- mean(u)
xbar #estimate value of log(2)
#checking answer
log(2)

#95% Confidence Interval
std.error <- 1.96 * sd(x) / sqrt(n)
std.error
CI.95 <- c(xbar - std.error, xbar + std.error)
names(CI.95) <- c("Lower Limit", "Upper Limit")
CI.95

#4.8.3
n <- 10000
u <- runif(n, 0, 1.96)
x <- 1.96*(1/sqrt(2*pi)) * exp((-1/2)*(u^2))
hist(x)
xbar <- mean(x)
xbar

  qqa#95% Confidence Interval
std.error <- 1.96 * sd(x) / sqrt(n)
std.error
CI.95 <- c(xbar - std.error, xbar + std.error)
names(CI.95) <- c("Lower Limit", "Upper Limit")
CI.95


#4.8.18
#doing simulation of 10,000 from U(0,1) for beta values of 1 to 10
my_MonteCarlo <- function(beta, n=10000){
  u <- runif(n, 0, 1)
  x <- u ^ (1/beta )
  mean(x)
}
beta <- 1:10
predicted_Value <- rep(NA, 10)
for(i in beta){
  predicted_Value[i] <- my_MonteCarlo(i)
}
predicted_Value
plot(beta, predicted_Value)



#example 