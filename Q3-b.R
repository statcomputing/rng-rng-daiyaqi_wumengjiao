my.alfa <- c(1.2, 1.5)
my.theta <- 2
my.beta <- 2
n <- 10000

u1 <- function(x){
  1/(2.4*(1 + x^2))
}
u2 <- function(y){
  sqrt(2 + y^2)/3
}

k <- sample.int(length(my.alfa), 10000, replace = TRUE, prob = my.alfa)
alfa1 <- c(my.theta, 1)
beta1 <- c(1, my.beta)
sample <- rbeta(n, alfa1[k], beta1[k])

rejsample <- function(x,y){
  if(any(x == 1)){
    u <- runif(1, 0, 1)
    if(any(u < u1(y)))
      return(y)
  }
  else{
    u <- runif(1, 0, 1)
    if(any(u < u2(y)))
      return(y)
  }
}

rejsample.list <- mapply(rejsample, k, sample)
rejsample.result <- matrix(unlist(rejsample.list), nr = 1) 
hist(rejsample.result, 100, freq = FALSE, xlab = "Value", main = "Rejection Sampling")
estimation <- density(rejsample.result)
lines(estimation, col = "red")
