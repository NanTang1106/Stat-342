# question 1
# a
n <- 100
mu <- 5
sample_dt <- rnorm(n, mu, 1)
summary(sample_dt)

#b
theta_value <- seq(4.7, 5.5, 0.001)
theta_density <- dnorm(theta_value, mean(sample_dt), sqrt(1/n))
plot(theta_value, theta_density, type='l', lwd=3, xlab='theta', ylab='Posterior Density', 
     main='Graph of Posterior Based on Sample Size 100')
abline(v=mean(sample_dt), lty=2, lwd=2)
legend('topright', 'Sample Mean', lty=2, lwd=2, cex=1)

# d
post_mu <- 5.085
post_sigma <- sqrt(1/100)
theta_dt <- rnorm(1000, post_mu, post_sigma)

theta_value <- seq(4.7, 5.5, 0.001)
post_density <- dnorm(theta_value, post_mu, post_sigma)

hist(theta_dt, breaks=20, probability=T, main='Histogram of Theta', 
     xlab='theta', ylab='Density')
lines(theta_value, post_density, lwd=3)
legend('topright', 'Posterior Density', lty=1, lwd=3, cex=1)

# question 5
# a
baby_dt <- read.table('babiesI.data', header=T, sep='')

smoke_wt <- baby_dt[baby_dt$smoke==1,]
summary(smoke_wt$bwt)

hist(smoke_wt$bwt, probability=T, xlab='Baby Weight',
     main='Histogram of Smoker Baby\'s Weight')
abline(v=88, lty=3, lwd=3)
legend('topright', legend='Low weight boundry', lty=3, lwd=3, cex=0.75)

# b
low_wt_rt <- nrow(smoke_wt[which(smoke_wt$bwt<88),]) / nrow(smoke_wt)





