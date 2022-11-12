library(datasets)  # Load/unload base packages manually

# The Result is the last x
newtonRapson <- function(x, n, f, df) {
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

df <- function(x) {
  return(-exp(1)^(-x))
}

newtonRapson(0, 10, f, df)
