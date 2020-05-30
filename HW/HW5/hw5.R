# question 1-b
n <- 1
sigma <- 1
tau0 <- 1
delta <- seq(-5, 5, by=0.01)
alpha <- 1 - pnorm(sigma * delta / (sqrt(n) * tau0^2))

plot(x=delta, y=alpha, type='l', lwd=3, xlab=expression(delta), ylab=expression(alpha),
     main='Test Size vs delta')
abline(h=c(0, 1), lty=3)

# question 2-c
x <- seq(0,1, 0.001)
y <- dbeta(x, 469/18, 937/18)
plot(x,y)

upper <- qbeta(0.975, 469/18, 937/18)
lower <- qbeta(0.025, 469/18, 937/18)

# question 3
theta <- seq(0, 1, 0.001)
post1 <- dbeta(theta, 469/18, 937/18)
post2 <- dbeta(theta, 22, 48)

plot(x=theta, y=post1, type='l', lwd=2, xlab='theta', ylab='posterior density', 
     main='Posterior Density with Different Priors')
lines(x=theta, y=post2, lty=3, lwd=2)
legend('topright', legend=c('Prior Beta(5.05, 5.05)', 'Prior Beta(1,1)'), 
       lty=c(1,3), lwd=3, cex=0.75)

upper <- qbeta(0.975, 22, 48)
lower <- qbeta(0.025, 22, 48)

# question 4
post <- dbeta(theta, 2,2)
plot(theta, post)

# question 5-a
norm_sp <- rnorm(n=25)
base_value <- seq(-3, 3, 0.01) 
density_value <- dnorm(base_value)

hist(norm_sp, xlim=c(-3, 3), probability=T, xlab='X', ylab='Density',
     main='Histogram of Normal Sample')
lines(base_value, density_value, lty=3, lwd=2)

qqnorm(norm_sp)
abline(a=0, b=1, lty=3)

# question 5-b
Z <- rnorm(n=25)
U <- runif(n=25)
Y <- Z/U
base_value <- seq(-8, 8, 0.01) 
density_value <- dnorm(base_value)

hist(Y, probability=T, xlim=c(-8, 8), ylim=c(0, 0.4))
lines(base_value, density_value, lty=3, lwd=2)

qqnorm(Y)
abline(a=0, b=1, lty=3)

