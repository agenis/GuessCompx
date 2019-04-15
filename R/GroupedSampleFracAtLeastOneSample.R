#' Sample a random proportion of the data, keeping at least one observation
#' @title Fraction Sampling without empty output
#' @param d_subset A dtta.frame from which a small sample is to be returned
#' @param prop A number between 0 and 1, being the desired sampling fraction.
#' @param is.random a boolean. If TRUE, a random sample is drawn, else it takes the head() of the data
#'
#' @return A random sample from the data, of proportion prop, but always returning at least one observation even if prop is too low.
#'
#' @details This function is designed to allow its use with group splitting or do.by methods.
#'
GroupedSampleFracAtLeastOneSample = function(d_subset, prop, is.random=TRUE){
  if (prop > 1){
    message("Warning: Proportion cannot be greater than one. Proportion was set to 1.")
    prop <- min(1, prop)
  }
  nb.to.sample = ifelse(prop * nrow(d_subset) < 1, 1, prop * nrow(d_subset))
  if (is.random==TRUE){
    return(d_subset %>% sample_n(nb.to.sample))
  } else {
    return(d_subset %>% head(nb.to.sample))
  }
}


