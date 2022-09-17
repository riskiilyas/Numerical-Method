library(datasets)  # Load/unload base packages manually

secant <- function(xmin, x, n, f) {
  fx <- c()
  xminList <- c()
  xList <- c()
  xPlusList <- c()
  for(i in 1:n) {
    xplus = x - (f(x)*(xmin-x)) / (f(xmin)-f(x))
    fx[i] <- format(round(f(xplus), 3), nsmall = 3)
    xminList[i] <- format(round(xmin, 3), nsmall = 3)
    xList[i] <- format(round(x, 3), nsmall = 3)
    xPlusList[i] <- format(round(xplus, 3), nsmall = 3)
    xmin = x
    x = xplus
  }
  
  iteration = c(1:n)
  plot(iteration, fx)
  
  data = matrix(c(xminList,xList,xPlusList,fx), n, 4)
  colnames(data) <- c('x-1', 'x','x+1', 'f(x)')
  rownames(data) <- c(1:n)
  
  table = as.table(data)
  
  table
  
}

f <- function(x) {
  return(exp(1)^(-x) - x)
}

secant(0, 1, 10, f)

