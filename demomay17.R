data<-c(14, 9.4, 12.1, 13.4, 6.3, 8.5, 7.1, 12.4, 13.3, 9.1)

n<-length(data)                             #sample size

xbar<-mean(data)
s<-sd(data)

(Rx<-(data - xbar)/s )  

# residual plot
plot(Rx)
abline(h=0)

# normal plot
qqnorm(Rx)
abline(a=0,b=1)

for (ii in 1:100) {
  sim <- rnorm(10)
  sp_sd <- sd(sim)
  sp_mean <- mean(sim)
  R_sp <- (sim- sp_mean)/sp_sd
  plot(R_sp)
  abline(h=0)
  Sys.sleep(0.1)
}

for (ii in 1:100) {
  sim <- rnorm(10)
  sp_sd <- sd(sim)
  sp_mean <- mean(sim)
  R_sp <- (sim- sp_mean)/sp_sd
  qqnorm(R_sp, ylim=c(-2, 2))
  abline(a=0, b=1)
  Sys.sleep(0.1)
}


#calculate R

(Dx<- -(1/n)*sum( log(Rx^2/(n-1))) )  #calculate discrepancy statistic

B<-10000                                  #large number of replications

simDvalues<-NULL                         #vector to store simulated values of 
                                        #the D statistic

for(k in 1:B){
  z<-rnorm(n=n,mean=0,sd=1)               #generate the standard normals
  zbar<-mean(z)                           # calculate their mean and 
  sz<-sd(z)              # sum of squares in preparation for
  r<-(z - zbar)/sz                        #calculating residuals r
  
  simDvalues<-rbind(simDvalues,(-1/n)*sum( log(r^2/(n-1))) )
  
} 

hist(simDvalues,prob=T,main="Sampling Distribution of D under normal model \ 
     (10,000 reps)",xlab="Values for D")

sum(simDvalues<=Dx)





