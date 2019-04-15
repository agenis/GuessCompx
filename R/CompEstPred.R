#' _Prediction function for the computation time of a whole dataset
#' @title Complexity Estimation and Prediction
#' @param model.list A list containing the fitted complexity functions, produced by CompEst()
#' @param benchmark A vector of LOO errors of complexity functions, produced by CompEst()
#' @param N number of rows of the whole dataset, produced by CompEst()
#' @param use a string indicatif if the function deals "time" or "memory" data
#'
#' @importFrom lubridate seconds_to_period
#'
#' @return a string of the predicted time for the whole dataset
#'
CompEstPred = function(model.list, benchmark, N, use="time"){
  if (names(which.min(benchmark))=="NlogN") {
    estimation <- predict(model.list[[which.min(benchmark)]], newdata = data.frame('NlogN_X' = N*log(N)))
  } else {
    estimation <- predict(model.list[[which.min(benchmark)]], newdata = data.frame('size' = N))
  }
  return(ifelse(use=="time", as.character(seconds_to_period(round(estimation, 2))), paste0(round(estimation), " Mb")))
}
