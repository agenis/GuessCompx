#' _Plot function for the results of algorithms complexity
#' @title Complexity Estimation and Prediction
#' @param to.plot a dataset produced by CompEst() function
#' @param element_title a string that will be added to the subtitle of the plot
#' @param use a string indicatif if the function deals "time" or "memory" data
#'
#' @import ggplot2
#'
#' @return a ggplot object
#'
CompEstPlot = function(to.plot, element_title = list("",""), use="time"){

  if (use=="time"){

    g <- ggplot(to.plot[to.plot$model!="time", ]) +
      aes(x=size, y=time, color=model) +
      geom_point() +
      geom_line(aes(size=best.model), alpha=0.8)  +
      geom_point(data=to.plot[to.plot$model=="time", ], aes(x=size, y=time), color="black", size=3, alpha=0.7) +
      scale_size_manual(values = c(.5, 3)) +
      scale_colour_brewer(palette = "Set1") +
      ggtitle(element_title[[1]], subtitle = element_title[[2]])


  } else if (use=="memory"){

    g <- ggplot(to.plot[to.plot$model!="memory", ]) +
      aes(x=size, y=memory, color=model) +
      geom_point() +
      geom_line(aes(size=best.model), alpha=0.8)  +
      geom_point(data=to.plot[to.plot$model=="memory", ], aes(x=size, y=memory), color="black", size=3, alpha=0.7) +
      scale_size_manual(values = c(.5, 3)) +
      scale_colour_brewer(palette = "Set1") +
      ggtitle(element_title[[1]], subtitle = element_title[[2]])

  }

  return(g)
}
