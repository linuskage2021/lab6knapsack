knapsack_dynamic <- function(x, W){

  n <- nrow(x)

  m <- matrix(data = 0, nrow = n, ncol = W)

  for(i in 2:n){

    for(j in 1:W){
      if(x[i,1] > j){
        m[i,j] <- m[i-1,j]
      }else{
        m[i,j] <- max(m[i-1,j], m[i-1,j-x$w[i]] + x$v[i])
      }
    }
  }



  n <- nrow(x)
  c <- W
  element <- vector()
  while(c > 0){
    if(m[n,c] != m[n-1,c]){
      element <- c(element, n)
      c <- c - x$w[n]



    }
    n <- n-1
    if(n == 1) break
  }

  return(list(value = round(max(m)), element = rev(element)))
}
