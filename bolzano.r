library(datasets)  # Load/unload base packages manually


bolzano <- function(a, b, n, f) {
  if(f(a) * f(b) > 0) {
    return()
  }
  
  xList <- c()
  x <- -1
  fx <- c()
  
  for (i in 1:n) {
    x <- (a+b)/2.0
    fx[i] <- format(round(f(x), 3), nsmall = 3)
    xList[i] <- format(round(x, 3), nsmall = 3)
    
    if(f(x) == 0) {
      break
    }
    
    if(f(a) * f(x) < 0) {
      b <- x
    } else if(f(b) * f(x) < 0) {
      a <- x
    }
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
  return(x^3 - x^2 + 2)
}

bolzano(-10, 0, 30, f)
