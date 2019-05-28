#' _Main function for the complexity estimation of an algorithm
#'
#' @title Complexity Estimation and Prediction
#'
#' @param d the data.frame, vector or matrix on which the algorithm is to be tested
#' @param f a user-defined function that runs the algorithm, taking d as first argument. No return value is needed.
#' @param random.sampling boolean; if TRUE a random sample is taken at each step, if FALSE the first N observations are taken at each step. Choosing a random sampling is relevant whith the use of replicates to help the discrimination power for complexity functions.
#' @param max.time maximum time in seconds allowed for each step of the analysis. The function will stop once this time limit has been reached. Default is 30 seconds. There is no such limitation regarding memory.
#' @param start.size the size in rows of the first sample to run the algorithm. Default is `floor(log2(nrow(d)))`. If strata is not NULL, we recommend to enter a multiple of the number of categories.
#' @param replicates the number of replicated runs of the algorithm for a specific sample size. Allows to better discriminate the complexity functions. Default is 2.
#' @param strata a string, name of the categorical column of d that must be used for stratified sampling. A fixed proportion of the categories will be sampled, always keeping at least one observation per category.
#' @param power.factor the common ratio of the geometric progression of the sample sizes. Default is 2, and will make sample sizes double every step. Decimal numbers are allowed.
#' @param alpha.value the alpha risk of the test whether the model is significantly different from a constant relation. Default is 0.005.
#' @param plot.result boolean indicatif if a summary plot of all the complexity functions is to be displayed
#'
#' @return A list with the best complexity function and the computation time on the whole dataset, for both time and memory complexity (Windows) and time complexity only (all other OS).
#'
#' @details The fit of a complexity function is one among: constant, linear, quadratic, cubic, logarithmic, square.root, n.log(n). Model comparison is achieved using Leave-One-Out error minimisation of the MSE (see `boot::cv.glm` doc). Note that when a CONSTANT relationship is predicted, it might simply mean that the max.time value is too low to show any tendency. For time series, the sampling removes the ts attribute to the input vector, so the user's function shall include again this ts() if a frequency is needed; also remind to avoid random sampling for it will break the series.
#'
#' @export
#' @import dplyr
#' @importFrom stats anova
#' @importFrom stats fitted
#' @importFrom utils memory.size
#' @importFrom utils tail
#' @importFrom utils head
#' @importFrom utils memory.limit
#' @importFrom reshape2 melt
#' @importFrom boot cv.glm
#'
#' @examples
#' # Dummy function that mimics a constant time complexity and
#' # N.log(N) memory complexity:
#' f1 = function(df){
#'   Sys.sleep(rnorm(1, 0.1, 0.02))
#'   v = rnorm(n = nrow(df)*log(nrow(df))*(runif(1, 1e3, 1.1e3)))
#' }
#' out = CompEst(d = mtcars, f = f1, replicates=2, start.size=2, max.time = 1)
#' # Raises an alert for TIME complexity.
#' # Sometimes confuses MEMORY complexity with linear:
#' print(out)
#'\dontrun{
#' # Real dist function analysis (everything is quadratic here):
#' f2 = dist
#' d  = ggplot2::diamonds[, 5:8]
#' CompEst(d = d, f = f2, replicates = 1, max.time = 1)
#'
#' # For time series functions, your `f` argument may include ts()
#' # to avoid loosing this ts attribute at sampling
#' # It is also recommended to set `start.size` argument to 3 periods at least.
#' f = function(d) arima(ts(d, freq = 12), order=c(1,0,1), seasonal = c(0,1,1))
#' d = ggplot2::txhousing$sales
#' # Should return a linear trend for TIME:
#' CompEst(d, f, start.size = 4*12, random.sampling = FALSE)
#' }
CompEst = function(d, f, random.sampling = FALSE, max.time = 30, start.size = NULL, replicates = 4, strata = NULL, power.factor = 2, alpha.value=0.005, plot.result = TRUE) {

  # is_myOS_windows <- Sys.info()["sysname"] %in% c("windows", "Windows", "WINDOWS")
  is_myOS_windows <- !is.infinite(suppressWarnings(memory.size(max = FALSE)))
  size <- NULL
  NlogN_X <- NULL
  model <- NULL
  dataset_name   <- deparse(substitute(d))
  algorithm_name <- deparse(substitute(f))

  if ((!is.null(strata)) & (!is.data.frame(d))){
    stop("Stratified sampling is only compatible with input data in the data.frame format")
  }

  # d <- data.frame(d) # change this?
  N <- NROW(d)
  if (is.null(start.size)){
    start.size <- floor(log2(NROW(d)))
  }

  if (!is.null(strata)) {
    if (!(strata %in% names(d))){
      stop(paste0("No column named *", strata, "* found in the data. Stopping the process."))
    } else {
      start.size = max(start.size, NROW(unique(d[strata])))
    }
  }

  default.sample.sizes <- start.size * rep(power.factor^(0:25), each = replicates)
  sample.sizes         <- floor(   append(default.sample.sizes[default.sample.sizes >= start.size & default.sample.sizes < N], rep(N, each = replicates))   )
  recorded.times       <- 0
  recorded.mems        <- 0
  i                    <- 1

  if (length(unique(sample.sizes)) < 3) {
    stop("Current configuration does not allow to compute more than 2 sample sizes and model fitting will fail. Stopping the process. Please increase max.time or decrease start.size.")
  }

  while(    (recorded.times[i] < max.time/power.factor/replicates) & (i < length(sample.sizes))   ){
    if (is.null(strata)){
      sampled.data     <- rhead(d, sample.sizes[i])
    } else {
      sampled.data     <- do.call("rbind", by(d, d[strata], GroupedSampleFracAtLeastOneSample, prop = sample.sizes[i]/N))
    }
    if (is_myOS_windows){
      gc(); gc();
      memory.before    <- memory.size()
      recorded.times   <- append(recorded.times, system.time(     f(sampled.data)     )[3])
      memory.after     <- memory.size()
      gc(); gc();
      recorded.mems    <- append(recorded.mems, memory.after - memory.before)
      i                <- i+1
    } else {
      gc(); gc();
      # memory.before    <- memory.size()
      recorded.times   <- append(recorded.times, system.time(     f(sampled.data)     )[3])
      # memory.after     <- memory.size()
      gc(); gc();
      # recorded.mems    <- append(recorded.mems, memory.after - memory.before)
      i                <- i+1
    }

  }
  recorded.times = tail(recorded.times, -1)
  if (is_myOS_windows) { recorded.mems  = tail(recorded.mems,  -1) }

  if (length(recorded.times) %in% c(1, 2)) {
    stop("The allowed max.time value is too small to run the algorithm on more than 2 sample sizes. Unable to proceed with cross-validation or model fitting. Stopping the process. Please increase max.time or decrease start.size.")
  }

  # TIME RESULTS

  temp        <- CompEstBenchmark(data.frame('size'   = head(sample.sizes, length(recorded.times)),
                                             'time'   = recorded.times,
                                             "memory" = recorded.mems) %>%
                                    mutate(NlogN_X = size*log(size)), use="time")
  model.list  <- temp[[1]]
  to.model    <- temp[[2]]
  benchmark   <- lapply(model.list, function(x) cv.glm(to.model, x)$delta[2])
  best.model  <- names(which.min(benchmark))
  if (best.model=="constant") message("Best model CONSTANT for time can be caused by not choosing a sufficiently high max.time value")
  full.time   <- CompEstPred(model.list, benchmark, N, use="time")
  signif.test <- tail(anova(model.list[[which.min(benchmark)]], test="F")$Pr, 1)
  uncertain   <- is.na(signif.test) | signif.test > alpha.value
  if ( uncertain ) message("warning: best TIME model not significantly different from a constant relationship. Increase max.time or replicates.")
  output.time = list(
    'best.model' = toupper(best.model),
    'computation.time.on.full.dataset' = full.time,
    'p.value.model.significance' = signif.test
  )
  if (plot.result==TRUE) {
    custom_titles = list("Complexity Fit against RUN TIME",   paste0("ALGORITHM: ", algorithm_name, "() // DATASET: ", dataset_name, " // STRATA: ", ifelse(is.null(strata), "None", strata))  )
    to.plot <- to.model %>% select(-NlogN_X) %>% melt(measure.vars=c(2, 4:10), value.name="time", variable.name="model") %>% mutate(best.model = model==best.model)
    print(CompEstPlot(to.plot, element_title = custom_titles))
  }

  # MEMORY RESULTS

  if (is_myOS_windows){
    temp        <- CompEstBenchmark(data.frame('size'   = head(sample.sizes, length(recorded.times)),
                                               'time'   = recorded.times,
                                               "memory" = recorded.mems) %>%
                                      mutate(NlogN_X = size*log(size)), use="memory")
    model.list  <- temp[[1]]
    to.model    <- temp[[2]]
    benchmark   <- lapply(model.list, function(x) cv.glm(to.model, x)$delta[2])
    best.model  <- names(which.min(benchmark))
    if (best.model=="constant") message("Best model CONSTANT for memory can be caused by not choosing a sufficiently high max.time value")
    full.memory <- CompEstPred(model.list, benchmark, N, use="memory")
    signif.test <- tail(anova(model.list[[which.min(benchmark)]], test="F")$Pr, 1)
    uncertain   <- is.na(signif.test) | signif.test > alpha.value
    if ( uncertain ) message("warning: best MEMORY model not significantly different from a constant relationship. Increase max.time or replicates.")
    output.memory = list(
      'best.model' = toupper(best.model),
      'memory.usage.on.full.dataset' = full.memory,
      'system.memory.limit' = paste0(memory.limit(), " Mb"),
      'p.value.model.significance' = signif.test
    )
    if (plot.result==TRUE) {
      custom_titles = list("Complexity Fit against MEMORY USAGE",   paste0("ALGORITHM: ", algorithm_name, "() // DATASET: ", dataset_name, " // STRATA: ", ifelse(is.null(strata), "None", strata))  )
      to.plot <- to.model %>% select(-NlogN_X) %>% melt(measure.vars=c(3, 4:10), value.name="memory", variable.name="model") %>% mutate(best.model = model==best.model)
      print(CompEstPlot(to.plot, element_title = custom_titles, use="memory"))
    }
  } else {
    output.memory = list("Warning" = "Memory analysis is only available for Windows OS")
  }

  return(list("sample.sizes" = sample.sizes,
              "TIME COMPLEXITY RESULTS" = output.time,
              "MEMORY COMPLEXITY RESULTS" = output.memory))
}

