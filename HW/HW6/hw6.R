# question 1-a
sp_dt <- c(14.0, 9.4, 12.1, 13.4, 6.3, 8.5, 7.1, 12.4, 13.3, 9.1)
n <- length(sp_dt)
B = 10000

D_values <- rep(NA, B)
for (ii in 1:B) {
  sim_z <- rnorm(n=10) 
  sim_resid <- sqrt(n-1) * (sim_z - mean(sim_z)) / 
    sqrt(sum((sim_z - mean(sim_z))^2))
  #sim_D <- - 1/n * sum(log(sim_resid^2 / 9))
  sim_D <- (n-1)^(-3/2) * sum(sim_resid^3)
  D_values[ii] <- sim_D
}

hist(D_values, probability=T, xlim=c(-1, 1), 
     xlab='D-skew values', main='Histogram of D-skew')

# question 1-b
sp_sd <- sd(sp_dt)
sp_resid <- sp_dt - mean(sp_dt)
ancillary_R <- sp_resid / sp_sd
sp_D <- (n-1)^(-3/2) * sum(ancillary_R^3)

p_value <- length(D_values[D_values < sp_D | D_values > abs(sp_D)]) / B



# question 4-a
setwd('/Users/nantang/Google Drive/STAT 342/HW/HW6')
baby_dt <- read.table('babiesI.data', header=T)
smoke_dt <- baby_dt[(baby_dt$smoke==1),]
nonsmoke_dt <- baby_dt[(baby_dt$smoke==0),]

# smoke
smoke_sd <- sd(smoke_dt$bwt)
smoke_mean <- mean(smoke_dt$bwt)
smoke_resid <- (smoke_dt$bwt - smoke_mean) / smoke_sd
smoke_D <- (n-1)^(-3/2) * sum(smoke_resid^3)

n <- nrow(smoke_dt)
B = 10000

D_values <- rep(NA, B)
for (ii in 1:B) {
  sim_z <- rnorm(n) 
  sim_resid <- sqrt(n-1) * (sim_z - mean(sim_z)) / 
    sqrt(sum((sim_z - mean(sim_z))^2))
  sim_D <- (n-1)^(-3/2) * sum(sim_resid^3)
  D_values[ii] <- sim_D
}

hist(D_values, probability=T, xlab='D-skew values', main='Histogram of D-skew (Smoker)')
abline(v=smoke_D, lwd=3, lty=3)

p_value <- length(D_values[D_values < smoke_D | D_values > abs(smoke_D)]) / B

#nonsmoke
nonsmoke_sd <- sd(nonsmoke_dt$bwt)
nonsmoke_mean <- mean(nonsmoke_dt$bwt)
nonsmoke_resid <- (nonsmoke_dt$bwt - nonsmoke_mean) / nonsmoke_sd
nonsmoke_D <- (n-1)^(-3/2) * sum(nonsmoke_resid^3)

n <- nrow(nonsmoke_dt)
B = 10000

D_values <- rep(NA, B)
for (ii in 1:B) {
  sim_z <- rnorm(n) 
  sim_resid <- sqrt(n-1) * (sim_z - mean(sim_z)) / 
    sqrt(sum((sim_z - mean(sim_z))^2))
  sim_D <- (n-1)^(-3/2) * sum(sim_resid^3)
  D_values[ii] <- sim_D
}

hist(D_values, probability=T, xlab='D-skew values', main='Histogram of D-skew (Non-Smoker)')
abline(v=nonsmoke_D, lwd=3, lty=3)

p_value <- length(D_values[D_values < nonsmoke_D | D_values > abs(nonsmoke_D)]) / B

# question 4-b
smoke_sd <- sd(smoke_dt$bwt)
smoke_mean <- mean(smoke_dt$bwt)

smoke_resid <- (smoke_dt$bwt - smoke_mean) / smoke_sd
plot(smoke_resid, pch=16, cex=0.5, ylab='Standardized Residuals', ylim=c(-3, 3),
     main='Smoker\'s Baby Weight Residual Plot')
abline(h=0, lty=1, lwd=1, col='gray')


nonsmoke_sd <- sd(nonsmoke_dt$bwt)
nonsmoke_mean <- mean(nonsmoke_dt$bwt)

nonsmoke_resid <- (nonsmoke_dt$bwt - nonsmoke_mean) / nonsmoke_sd
plot(nonsmoke_resid, pch=16, cex=0.5, ylab='Standardized Residuals', ylim=c(-4, 4),
     main='Non-Smoker\'s Baby Weight Residual Plot')
abline(h=c(0, -3, 3), lty=1, lwd=1, col='gray')

# question 4-c
qqnorm(smoke_resid, cex=0.8, main='Normal QQ-Plot for Smoker\'s Baby Weight', ylim=c(-3,3))
abline(a=0, b=1)

qqnorm(nonsmoke_resid, cex=0.8, main='Normal QQ-Plot for Non-Smoker\'s Baby Weight', ylim=c(-3,3))
abline(a=0, b=1)

# question 5-a
p_value_dt <- c(0.01, 0.01, 0.02, 0.04, 0.04, 0.05, 0.07, 0.07, 0.10, 0.19,
                0.24, 0.27, 0.34, 0.37, 0.44, 0.50, 0.53, 0.54, 0.55, 0.61,
                0.70, 0.77, 0.80, 0.80, 0.82, 0.94, 0.99)
n <- length(p_value_dt)
expect_qt <- seq(1, n) / (n + 1)  
qqplot(expect_qt, p_value_dt, xlab='Theoratical Quantiles', ylab='Observed Quantiles',
       main='QQ-Plot of P-values')
abline(a=0,b=1)

# question 5-b
hist(p_value_dt, breaks=5)
interval_count <- c(10, 4, 5, 5, 3)
chisq.test(interval_count, p=rep(0.2, 5))




  
  
  
  
