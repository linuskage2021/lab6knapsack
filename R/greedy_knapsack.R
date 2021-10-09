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

