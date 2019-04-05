#6.3.2
library(ggplot2)

n <- 10
alpha <- 0.05
H0 <- 1
Theta <- seq(0.001, 6, by=1e-3)
Theta

T <- dchisq(2*n, alpha / 2)


c1 <-  H0 / Theta * qchisq(p = alpha / 2, df=2*n)
length(c1)
c2 <- H0 / Theta * qchisq(p = 1 - alpha / 2, df=2*n)
head(c2)

Power <- pchisq(q=c1, df=2*n) + pchisq(q=c2, df=2*n, lower.tail=F)


Power[which(Theta==1)] #Power is equivolent to size when Theta=H0

data <- as.data.frame(cbind(Power, Theta))
#plot(Theta, Power, type="l", xlab="Theta")
ggplot(data=as.data.frame(data), aes(x=Theta, y=Power)) +
  geom_line(color="blue", size=1)+
  labs(title="Power Function",x="Theta Under the Alternate Hypothesis", y = "Power of the Test")+theme_minimal()
