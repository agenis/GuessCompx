
#' Small Random Sample from a vector or data.frame
#' @title Random Head
#' @param data A vector or data.frame from which a small sample is to be returned
#' @param rows A positive integer, representing the number of lines or elements to print, default is 7.
#' @param is.random a boolean. If TRUE, a random sample is drawn, else it takes the head() of the data
#'
#' @return A random sample without replacement taken from the data, in the same format than the input.
#'
#' @details If the rows parameter is greater than the actual number of rows/elements of the data, the returned value is the initial dataset after shuffling
#'
#' @export
#'
#' @examples
#' set.seed(1234)
#' rhead(mtcars)
rhead = function(data, rows=7, is.random=TRUE) {
  if (rows>NROW(data)){
    message(paste0("Sample size is greater than the data: returning the initial data", ifelse(is.random, " after shuffling", "")))
    rows = NROW(data)
    }
  if(is.null(dim(data))) # vector
    if (is.random==TRUE){
      return(data[base::sample(NROW(data), rows) ])
    } else {
      return(data[1:rows])
    }

  if (is.random==TRUE){
    return(data[base::sample(NROW(data), rows), ])
  } else {
    return(data[1:rows, ])
  }
}





