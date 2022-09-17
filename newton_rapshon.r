library(datasets)  # Load/unload base packages manually


# wrapper around sprintf() and print()
printf <- function(...) invisible(cat(sprintf(...)))
print <- function(...) invisible(cat((...), "\n"))

# f(x) = e^(-x) - x
f <- function(x) {
  return(exp(1)^(-x) - x)
}

# f: function
# x_min1, xi: initial x value (x-1, x0)
# n: no. of iterations to perform
secant <- function(f, xi_min1, xi, n) {
  trials <- c()
  x <- c()
  fx <- c()
  for (i in 1:n) {
    printf("%03d | x = %.8f | f(x) = %.8f\n", i, xi, f(xi))
    trials[i] = i
    x[i] = xi
    fx[i] =  f(xi)
    x_plus1 <- xi - ((f(xi) * (xi_min1 - xi)) / (f(xi_min1) - f(xi)))
    xi_min1 <- xi
    xi <- x_plus1
  }
  
  #datas = matrix(c(xCol,fxCol), ncol = 2, nrow = n)
  #colnames(datas) = c('x', 'F(x)')
  #rownames(datas) = c(trials)
  
  plot(fx, x)
}

secant(f, 0, 1, 10)

