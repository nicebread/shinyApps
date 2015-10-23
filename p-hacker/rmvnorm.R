library(mvtnorm)
n <- 3
p <- 0.5
sigma <- matrix(p, nrow=n, ncol=n)
diag(sigma) <- 1.0
data <- rmvnorm(10000, sigma=sigma)
cor(data)

