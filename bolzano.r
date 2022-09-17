library(datasets)  # Load/unload base packages manually


bolzano <- function(a, b, n, f) {
  if(f(a) * f(b) > 0) {
    return()
  } else if(f(a) == 0) {
    return(a)
  } else if(f(b) == 0) {
    return(b)
  }
  
  c <- -1
  fx <- c()
  
  for (i in 1:n) {
    c <- (a+b)/2.0
    fx[i] = c
    
    if(f(c) == 0) {
      return(c)
    }
    
    if(f(a) * f(c) < 0) {
      b <- c
    } else if(f(b) * f(c) < 0) {
      a <- c
    } else {
      return()
    }
  }
  
  
  plot(fx, c)
  print(c)
}

f <- function(x) {
  return(x^3 - x^2 + 2)
}

bolzano(-200, 0, 30, f)
