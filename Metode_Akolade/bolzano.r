library(datasets)  # Load/unload base packages manually


bolzano <- function(a, b, n, f) {
  if(f(a) * f(b) > 0) {
    return()
  }
  
  x3List <- c()
  x3 <- -1
  x1 <- c()
  x2 <- c()
  fx <- c()
  
  for (i in 1:n) {
    x3 <- (a+b)/2.0
    a_ = format(round(a, 3), nsmall = 3)
    b_ = format(round(b, 3), nsmall = 3)
    x1[i] <- ifelse(a<b, a_,b_)
    x2[i] <- ifelse(a<b, b_,a_)
    fx[i] <- format(round(f(x3), 3), nsmall = 3)
    x3List[i] <- format(round(x3, 3), nsmall = 3)
    
    if(f(x3) == 0) {
      break
    }
    
    if(f(a) * f(x3) < 0) {
      b <- x3
    } else if(f(b) * f(x3) < 0) {
      a <- x3
    }
  }
  
  iteration = c(1:n)
  plot(iteration, fx)
  
  data = matrix(c(x1,x2,x3List,fx), n, 4)
  colnames(data) <- c('x1', 'x2','x3', 'f(x)')
  rownames(data) <- c(1:n)
  
  table = as.table(data)
  
  table
}

f <- function(x) {
  return(x^3 - x^2 + 2)
}

bolzano(-10, 0, 30, f)
