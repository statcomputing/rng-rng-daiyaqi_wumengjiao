# Part (b)

# density
dmixgamma <- function(x, pi, alpha, beta) {
  k <- length(pi)
  n <- length(x)
  rowSums(vapply(1:k, function(i) pi[i]*dgamma(x, alpha[i], beta[i]), numeric(n)))
}

# random generation
rmixgamma <- function(n, pi, alpha, beta) {
  k <- sample.int(length(pi), n, replace = TRUE, prob = pi)
  rgamma(n, alpha[k], beta[k])
}

set.seed(123)

theta <- 3
weight <- 2*gamma(theta)/(2*gamma(theta)+gamma(theta+0.5))
pi <- c(weight, 1-weight)
alpha <- c(theta, theta+0.5)
beta <- c(1, 1)
sample <- rmixgamma(1e4, pi, alpha, beta)

KDE <- density(sample)
hist(sample, 100, freq = FALSE, main = "Kernel density estimation and True density", 
     xlab = "Value", ylab = "Density")
lines(KDE, col = "red")
xx <- seq(0, 14, by = 0.001)
lines(xx, dmixgamma(xx, pi, alpha, beta), col = "blue")
legend("topright", c("KDE", "True density"), lty = c(1,1), col = c("red", "blue"))



# Part (c)

f <- function(x) {sqrt(4+x)*x^(theta-1)*exp(-x)}
g <- function(x) {(2*x^(theta-1)+x^(theta-0.5))*exp(-x)}
simu <- function (x) {
  u <- runif(1, 0, g(x))
  if (any(u < (1/1.2)*f(x)))
      return(x)
  }

simu.list <- sapply(sample, simu)
simu.result <- matrix(unlist(simu.list), nr = 1) 
hist(simu.result, 100, freq = FALSE, main = "Rejection Sampling", xlab = "Value")
estimation <- density(simu.result)
lines(estimation, col = "red")
