#' Brute force method for knapsack problem.
#'
#' @param x is a data.frame containing two columns, w and v.
#' @param W is the capacity of the knapsack and is an integer greater than 0.
#'
#' @return Returns a list containing the maximum value of combination of the objects and the objects in the combination.
#' @export
#'
#' @examples
#' #'suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
#'set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#'n <- 2000
#'knapsack_objects <-
#'  data.frame(
#'    w=sample(1:4000, size = n, replace = TRUE),
#'    v=runif(n = n, 0, 10000)
#'  )
#'
#' brute_force_knapsack(knapsack_objects[1:8,], W = 3500)
brute_force_knapsack <- function(x, W){

  stopifnot(W > 0)
  if(is.data.frame(x) != TRUE) stop("The data input is not a data.frame")
  if(!identical(colnames(x), c("w", "v"))) stop("The columns names in data is wrong.")





  n <- nrow(x)

  A <- matrix(0, nrow = n, ncol = 2^n)
  t_elements <- vector(mode = "numeric")
  bestValue <- 0
  bestWeigth <- 0


  for(j in 1:2^n){
    A[,j] <- as.numeric(intToBits(j))[1:n]
  }


  for(i in 1:2^n){
    tempWeight <- 0
    tempValue <- 0
    elements <- c()


    for(k in 1:n){

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

