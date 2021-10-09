#' Greedy knapsack solution of the knapsack problem
#'
#' @param x x is a data.frame containing two columns, w and v
#' @param W W is the capacity of the knapsack and is an integer greater than 0.
#'
#' @return Returns a list containing the maximum value of combination of the objects and the objects in the combination.
#' @export
#'
#' @examples
#' #' #'suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
#'set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#'n <- 2000
#'knapsack_objects <-
#'  data.frame(
#'    w=sample(1:4000, size = n, replace = TRUE),
#'    v=runif(n = n, 0, 10000)
#'  )
#'
#' greedy_knapsack(knapsack_objects[1:800,], W = 3500)
greedy_knapsack <- function(x, W){


  stopifnot(W > 0)
  if(is.data.frame(x) != TRUE) stop("The data input is not a data.frame")
  if(!identical(colnames(x), c("w", "v"))) stop("The columns names in data is wrong.")



  x$ratio <- x$v/x$w

  x <- x[order(x$ratio, decreasing = TRUE),]

  sum_weigth <- 0
  sum_value <- 0

  index <- c()
  for(i in 1:length(x$ratio)){
    if(sum_weigth < W){
      sum_weigth <- sum_weigth + x$w[i]
      if(sum_weigth > W) break
      sum_value <- sum_value + x$v[i]


      index <- c(index, as.numeric(rownames(x[i,])))
      # print(x$v[i])
    }

  }

  ret_list <- list(value = round(sum_value), elements = index)
  return(ret_list)
}

