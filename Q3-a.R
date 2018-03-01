# Part (a)

# random generation
rmixbeta <- function(n, pi, alpha, beta) {
  k <- sample.int(length(pi), n, replace = TRUE, prob = pi)
  rbeta(n, alpha[k], beta[k])
}

set.seed(123)

theta <- 2
vbeta <- 2
pi <- c(beta(theta,1), 2*beta(1,vbeta))
alpha <- c(theta, 1)
beta <- c(1, vbeta)
sample <- rmixbeta(1e4, pi, alpha, beta)

f <- function(x) {x^(theta-1)/(1+x^2)+sqrt(2+x^2)*(1-x)^(beta-1)}
g <- function(x) {x^(theta-1)+2*(1-x)^(beta-1)}
simu <- function (x) {
  u <- runif(1, 0, g(x))
  if (any(u < f(x)))
    return(x)
}

simu.list <- sapply(sample, simu)
simu.result <- matrix(unlist(simu.list), nr = 1) 
hist(simu.result, 100, freq = FALSE, main = "Rejection Sampling", xlab = "Value")
estimation <- density(simu.result)
lines(estimation, col = "red")

