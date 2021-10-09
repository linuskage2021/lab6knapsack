brute_force_knapsack <- function(x, W){

  stopifnot(W > 0)
  if(is.data.frame(x) != TRUE) stop("The data input is not a data.frame")
  if(!identical(colnames(x), c("w", "v"))) stop("The columns names in data is wrong.")





  n <- nrow(x)

  A <- matrix(0, nrow = n, ncol = 2^n)
  t_elements <- vector(mode = "numeric")
  bestValue <- 0
  bestWeigth <- 0
  #Skapar matrisen med massa 0or men n rader, 2^n columner.
  for(j in 1:2^n){
    A[,j] <- as.numeric(intToBits(j))[1:n]
  }


  for(i in 1:2^n){ #i är columns
    tempWeight <- 0
    tempValue <- 0
    elements <- c()


    for(k in 1:n){ #k är rad.

      if(A[k,i] == 1){
        tempWeight <- tempWeight + x$w[k]
        if(tempWeight > W){break}
        tempValue <- tempValue + x$v[k]
        elements <- c(elements,k)

      }

    }

    if(tempValue > bestValue & tempWeight <= W){
      t_elements <- elements
      bestValue <- tempValue
      bestWeight <- tempWeight
    }


  }

  return(list(value = round(bestValue), elements = t_elements))
}

