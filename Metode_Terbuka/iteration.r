library(datasets)  # Load/unload base packages manually

# Change f(x) = 0 to x = f(x) first!
# The Result is the last x
iteration <- function(x, n, f){
  xList <- c()
  fx <- c()
  for(i in 1:n) {
    xList[i] <- format(round(x, 3), nsmall = 3)
    fx[i] <- format(round(f(x), 3), nsmall = 3)
    x <- f(x)
  }
  
  iteration = c(1:n)
  plot(iteration, fx)
  
  data = matrix(c(xList,fx), n, 2)
  colnames(data) <- c('x', 'f(x)')
  rownames(data) <- c(1:n)
  
  table = as.table(data)
  
  table
}

f <- function(x) {
  return(exp(1)^(-x))
}

# no.1
fx <- function(x) {
  return(x^3 + 6.6*x^2 - 29.05*x + 22.64) 
}

iteration(100, 10, fx)
