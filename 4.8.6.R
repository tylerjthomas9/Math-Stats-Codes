#f(x)=4x^3, 0<x<1
#F(x)=x^4, 0<x<1
#x = u^(1/4) = F^(-1)(u)

set.seed(12345)

my_simulation <- function(n = 100){
  u <- runif(n, 0, 1)
  x <<- u ^ (1 / 4)
  hist(x)
  mean(x)
  var(x)
}
my_simulation()
x
my_simulation(1000)
my_simulation(10000)