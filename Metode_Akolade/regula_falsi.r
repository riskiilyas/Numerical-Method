library(datasets)  # Load/unload base packages manually

regula_falsi <- function(x1, x2, n, f) {
  fx <- c()
  x1L <- c()
  x2L <- c()
  x3L <- c()
  for(i in 1:n) {
    x3 = (x1*f(x2) - x2*f(x1)) / (f(x2)-f(x1))
    fx[i] <- format(round(f(x3), 3), nsmall = 3)
    x1L[i] <- format(round(x1, 3), nsmall = 3)
    x2L[i] <- format(round(x2, 3), nsmall = 3)
    x3L[i] <- format(round(x3, 3), nsmall = 3)
    
    if(f(x3)*f(x1) < 0) {
      x2 = x3
    } else {
      x1 = x3
    }
  }
  
  iteration = c(1:n)
  plot(iteration, fx)
  
  data = matrix(c(x1L,x2L,x3L,fx), n, 4)
  colnames(data) <- c('x1', 'x2','x3', 'f(x)')
  rownames(data) <- c(1:n)
  
  table = as.table(data)
  
  table
}

f <- function(x) {
  return(exp(1)^(-x) - x)
}

regula_falsi(0, 1, 10, f)