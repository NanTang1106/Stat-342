normal_test <- function(x, nx, y, my) {
  n <- nx
  m <- my
  p1 <- x/nx
  p2 <- y/my
  sd <- sqrt(p1 * (1 - p1) / n + p2 * (1 - p2) / m)
  z <- (p1-p2) /sd
  return(z)
}

cle <- normal_test(45, 80, 26, 75)
cle_p <- (1 - pnorm(cle)) * 2
cle_bf <- cle_p * 4
cle_b <- cle_p * 4

dim <- normal_test(45, 80, 52, 85)
dim_p <- pnorm(dim) * 2
dim_bf <- 1
dim_b <- dim_p * 3/4

pen100 <- normal_test(45, 80, 35, 67)
pen100_p <- (1 - pnorm(pen100)) * 2
pen100_bf <- 1

pen150 <- normal_test(45, 80, 37, 85)
pen150_p <- (1 - pnorm(pen150)) * 2
pen150_b <- pen150_p * 2


 
