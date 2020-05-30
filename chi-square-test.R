library(stats4)

sp <- c(1.5, 1.6, 1.4, 9.7, 12.1, 2.7, 2.2, 1.6, 6.8, 0.1, 0.8, 1.7, 8.0, 0.2, 12.3, 2.2, 0.2, 0.6, 10.1, 4.9)
hist(sp)
count <- c(10, 3, 1, 2, 1, 3)

logtheta <- function (theta) {
  p0 <- 1 - exp(-2 * theta)
  p1 <- exp(-2 * theta) - exp(-4 * theta)
  p2 <- exp(-4 * theta) - exp(-6 * theta)
  p3 <- exp(-6 * theta) - exp(-8 * theta)
  p4 <- exp(-8 * theta) - exp(-10 * theta)
  p5 <- exp(-10 * theta)
  x0 <- count[1]
  x1 <- count[2]
  x2 <- count[3]
  x3 <- count[4]
  x4 <- count[5]
  x5 <- count[6]
  loglike <- suppressWarnings( - x0 * log(p0) - x1 * log(p1) - x2 * log(p2) 
                              - x3 * log(p3) - x4 * log(p4) - x5 * log(p5))
}

mle(logtheta, start=list(theta=2))

theta <- 0.2244758 
p0 <- 1 - exp(-2 * theta)
p1 <- exp(-2 * theta) - exp(-4 * theta)
p2 <- exp(-4 * theta) - exp(-6 * theta)
p3 <- exp(-6 * theta) - exp(-8 * theta)
p4 <- exp(-8 * theta) - exp(-10 * theta)
p5 <- exp(-10 * theta)

prob <- c(p0, p1, p2, p3, p4, p5)
expect <- prob * 20

D <- sum((count - expect)^2/expect)

1-pchisq(D, df=4)

chisq.test(x=count, p=prob)











