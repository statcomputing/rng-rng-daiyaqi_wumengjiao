# Plot f(x) and g(x)

theta <- 2
beta <- 2
x <- seq(0, 1, by = 0.0001)
f <- function(x) {x^(theta-1)/(1+x^2)+sqrt(2+x^2)*(1-x)^(beta-1)}
g <- function(x) {x^(theta-1)+2*(1-x)^(beta-1)}
plot(x, g(x), ylim = c(0,3), type = 'l', main = "Target and Envelope function", 
     ylab = "f(x) and g(x)", col = "red")
lines(x, f(x), col = "black", lty = 1)
legend("topright", legend = c("Envelope function g(x)", "Target function f(x)"), 
       lty = c(1,1), col = c("red", "black"))