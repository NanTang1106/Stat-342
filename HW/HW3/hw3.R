# Question 3
library(ggplot2)
theme_set(theme_bw())

baby_dt <- read.table("babiesI.data", sep = "", header=T)
baby_dt <- baby_dt[(baby_dt$smoke==0 | baby_dt$smoke==1),]

# histogram of babyweight
hist(baby_dt[baby_dt$smoke==1,]$bwt, xlim=c(min(baby_dt$bwt),max(baby_dt$bwt)), 
     ylim=c(0, 0.027), breaks=30, probability = T, col=rgb(1,0,0,0.5),
     main='Distribution of Baby Weight', xlab='Baby Weight')
hist(baby_dt[baby_dt$smoke==0,]$bwt, probability = T, breaks=30, add=T, 
     col=rgb(0,0,1,0.5))
legend('topleft', c('Smoke', 'Non-Smoke'), 
       col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),lwd=3, cex = 0.7)

status <- rep(NA, nrow(baby_dt))
for (i in 1:nrow(baby_dt)) {
  if(baby_dt[i,]$smoke) {
    status[i] = 'smoke'
  } else {
    status[i] = 'non-smoke'
  } 
}

baby_dt$status <- status
# Boxplot of babyweight
p <- ggplot(baby_dt, aes(status, bwt))
p + geom_boxplot(fill='skyblue') +
  labs(title="Boxplot of Baby Weight", 
       x="Smoke Status",
       y="Baby Weight") 


count <- 0
for(i in baby_dt[baby_dt$smoke==0,]$bwt) {
  if(i > 165.5) {
    count <- count + 1
  }
}
count
       