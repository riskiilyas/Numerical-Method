library(datasets)  # Load/unload base packages manually

# wrapper around sprintf() and print()
printf <- function(...) invisible(cat(sprintf(...)))
print <- function(...) invisible(cat((...), "\n"))

tabulasi <- function(x0, xn, n, f) {
  xSolution <- c()
  fxSolution <- c()
  for(i in 1:n) {
    div <- (xn - x0) / 10
    x <- c()
    fx <- c()
    
    for(j in 0:10) {
      x[j+1] <- x0 + j*div
      fx[j+1] <- f(x0 + j*div)
    }
    
    flag <- 0

    printf("Iteration #%d\n", i)    
    for(j in 1:11) {
      printf("x = %.3f | f(x) = %.3f\n", x[j], fx[j])
      if(j==11) break
      
      if(fx[j]*fx[j+1] < 0) {
        x0 = x[j]
        xn = x[j+1]
        flag = 1
        
        xSolution[i] = x0
        fxSolution[i] = fx[j]
      }
    }
    printf("Taken x : %.3f\n\n", x0)

    if(flag==0) break
  }
  
  iteration = c(1:n)
  plot(iteration, fxSolution)
}

f <- function(x) {
  return(exp(1)^(-x) - x)
}

tabulasi(0, 1, 10, f)
