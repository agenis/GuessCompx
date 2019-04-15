#' _Benchmark procedure to fit complexity functions to a data.frame of time or memory values
#' @title Complexity Estimation and Prediction
#' @param to.model A data.frame produced by the CompEst() function, comprised of the following columns: size, time, memory, NlogN_X
#' @param use a string indicatif if the function deals "time" or "memory" data
#'
#' @import dplyr
#'
#' @return a list with all the fitted complexity model.
#'
CompEstBenchmark = function(to.model, use="time"){

  if (use=="time"){

    constant    <- glm(time~1,          data=to.model); to.model['constant'] = fitted(constant)
    linear      <- glm(time~size,       data=to.model); to.model['linear'] = fitted(linear)
    quadratic   <- glm(time~I(size^2),  data=to.model); to.model['quadratic'] = fitted(quadratic)
    cubic       <- glm(time~I(size^3),  data=to.model); to.model['cubic'] = fitted(cubic)
    squareroot  <- glm(time~sqrt(size), data=to.model); to.model['squareroot'] = fitted(squareroot)
    log         <- glm(time~log(size),  data=to.model); to.model['log'] = fitted(log)
    NlogN       <- glm(time~NlogN_X,    data=to.model); to.model['NlogN'] = fitted(NlogN)

    model.list <- list('constant'   = constant,
                       'linear'     = linear,
                       'quadratic'  = quadratic,
                       'cubic'      = cubic,
                       'squareroot' = squareroot,
                       'log'        = log,
                       'NlogN'      = NlogN)

  } else if (use=="memory"){

    constant    <- glm(memory~1,          data=to.model); to.model['constant'] = fitted(constant)
    linear      <- glm(memory~size,       data=to.model); to.model['linear'] = fitted(linear)
    quadratic   <- glm(memory~I(size^2),  data=to.model); to.model['quadratic'] = fitted(quadratic)
    cubic       <- glm(memory~I(size^3),  data=to.model); to.model['cubic'] = fitted(cubic)
    squareroot  <- glm(memory~sqrt(size), data=to.model); to.model['squareroot'] = fitted(squareroot)
    log         <- glm(memory~log(size),  data=to.model); to.model['log'] = fitted(log)
    NlogN       <- glm(memory~NlogN_X,    data=to.model); to.model['NlogN'] = fitted(NlogN)

    model.list <- list('constant'   = constant,
                       'linear'     = linear,
                       'quadratic'  = quadratic,
                       'cubic'      = cubic,
                       'squareroot' = squareroot,
                       'log'        = log,
                       'NlogN'      = NlogN)

  }

  return(list(model.list, to.model))
}
