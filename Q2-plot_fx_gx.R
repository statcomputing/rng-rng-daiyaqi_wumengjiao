# Plot f(x) and 1.2g(x)

theta <- 3
x <- seq(0, 14, by = 0.001)
f <- function(x) {sqrt(4+x)*x^(theta-1)*exp(-x)}
g <- function(x) {(2*x^(theta-1)+x^(theta-0.5))*exp(-x)}
plot(x, 1.2*g(x), type = 'l', main = "Target and Envelope function", 
     ylab = "f(x) and 1.2g(x)", col = "red")
lines(x, f(x), col = "black", lty = 1)
legend("topright", legend = c("Envelope function 1.2g(x)", "Target function f(x)"), 
       lty = c(1,1), col = c("red", "black"))
