library(ggplot2)

## question 4-1
arc_data <- read.csv('arc.csv')

theme_set(theme_bw())
g <- ggplot(data = assessment_data, aes(DoctorA, DoctorB)) +
  geom_point() +
  geom_smooth(method = 'lm',se=F, linetype='solid') +
  geom_smooth(method = 'loess', se=F, linetype='dotted') +
  labs(x='Doctor A Assessment',
       y='Doctor B Assessment',
       title='Relation Between Two Assessments')

print(g)

cor(assessment_data$DoctorA, assessment_data$DoctorB)


## question 4-3
t.test(arc_data$DoctorA, arc_data$DoctorB, alternative = 'two.sided', paired = T)



## question 2-b
iteration <- 10000
lambda0 <- 1
sample_size <- 20
test_size <- 0.05
critical_value <- abs(qnorm(test_size/2))
count <- 0

for (i in 1:iteration) {
  sample <- rpois(sample_size, lambda0)
  W <- (sum(sample) - sample_size * lambda0) / sqrt(sum(sample))
  if (abs(W) > critical_value) {
    count <- count + 1
  } 
}

type_one_err <- count / iteration
print(type_one_err)

