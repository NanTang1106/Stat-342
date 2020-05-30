library(ggplot2)

# for mu range from 100 to 110
mu.values <- seq(100, 110, 0.01)

# calculate power curve for given n, sigma, mu
# under alpha level 0.05
get.power.curve <- function(n1, sigma1, mu1) {
  k <- 100 + 1.645 * sigma1 / sqrt(n1)
  return(1 - pnorm(sqrt(n1) * (k - mu1) / sigma1))
}

# power curve when n = 25, sigma = 14
pc1 <- get.power.curve(25, 14, mu.values)

# power curve when n = 100, sigma = 14
pc2 <- get.power.curve(100, 14, mu.values)

# power curve when n = 25, sigma = 28
pc3 <- get.power.curve(25, 28, mu.values)

pc.combine <- data.frame(mu.values, pc1, pc2, pc3)

powercurve <- ggplot(data = pc.combine) +
  geom_line(aes(x=mu.values, y=pc1, linetype='n = 25, sigma = 14'), size=1) +
  geom_line(aes(x=mu.values, y=pc2, linetype='n = 100, sigma = 14'), size=1) +
  geom_line(aes(x=mu.values, y=pc3, linetype='n = 25, sigma = 28'), size=1) +
  labs(y="Power", 
       x="mu", 
       title="Power Curves (mu ranges from 100 - 110)",
       subtitle="Under the significance level of 0.05",
       linetype='n, sigma values'
       ) +
  scale_linetype_manual(values=c("solid", "dotted", "dotdash"))

theme_set(theme_bw())
plot(powercurve)

bin.value <- rep(NA, 1000)
for(i in 1:1000) {
  sample <- rbinom(1, 10, 0.5)
  bin.value[i] = sample
}
hist(bin.value, probability = T)


(choose(14, 9)*choose(16, 4) + choose(14, 10)*choose(16, 3) +
  choose(14, 11)*choose(16, 2) + choose(14, 12)*choose(16, 1) +
  choose(14, 13)*choose(16, 0))/choose(30, 13)

