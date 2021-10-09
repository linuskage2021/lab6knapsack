#' Dynamic programming solution of knapsack problem
#'
#' @param x x is a data.frame containing two columns, w and v.
#' @param W W is the capacity of the knapsack and is an integer greater than 0.
#' @param fast is a logical argument. If TRUE, Rcpp code is run speeding up the calculations.
#'
#' @return Returns a list containing the maximum value of combination of the objects and the objects in the combination.
#' @export
#'
#' @examples
#'
#'
#'suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
#'set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#'n <- 2000
#'knapsack_objects <-
#'  data.frame(
#'    w=sample(1:4000, size = n, replace = TRUE),
#'    v=runif(n = n, 0, 10000)
#'  )
#'
#' #Run with C++ code.
#' knapsack_dynamic(knapsack_objects[1:8,], W = 3500, fast = TRUE)
#'
#' #Run without C++ code.
#' knapsack_dynamic(knapsack_objects[1:8,], W = 3500, fast = FALSE)
knapsack_dynamic <- function(x, W, fast = FALSE){

  n <- nrow(x)

  m <- matrix(data = 0, nrow = n, ncol = W)

  stopifnot(W > 0)
  if(is.data.frame(x) != TRUE) stop("The data input is not a data.frame")
  if(!identical(colnames(x), c("w", "v"))) stop("The columns names in data is wrong.")



  if(fast){
    func <- Rcpp::cppFunction('NumericMatrix func(NumericMatrix x, int W, NumericMatrix m) {
  int nrow = x.nrow(), ncol = W;
  for (int i = 1; i < nrow; i++) {

    for (int j = 0; j < ncol; j++) {

        if(x(i,0) > j){
          m(i,j) = m(i-1,j);
        }else{
          m(i,j) = std::max(m(i-1,j), m(i-1,j-x(i,0)) + x(i,1));
        }


    }

  }
  return m;
}')

    x1 <- as.matrix(x)
    m <- func(x1, W, m)

  }else{


    for(i in 2:n){

      for(j in 1:W){
        if(x[i,1] > j){
          m[i,j] <- m[i-1,j]
        }else{
          m[i,j] <- max(m[i-1,j], m[i-1,j-x$w[i]] + x$v[i])
        }
      }
    }


  }




  n <- nrow(x)
  c <- W
  element <- vector()
  while(c > 0){
    if(m[n,c] != m[n-1,c]){
      element <- c(element, n)
      c <- c - x[n,1]



    }
    n <- n-1
    if(n == 1) break
  }


  return(list(value = round(max(m)), elements = rev(element)))
}




