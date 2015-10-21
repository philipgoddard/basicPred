#' @include modelPred_class.R
NULL

#' Show method for modelPredictions
#'
#' @export
#'
setMethod(
  f = "show",
  signature = "modelPredictions",
  function(object) {

    # may consider transforming predicted classes from binary to actual class names?

    cat("*** Class modelPredictions, method show *** \n")
    cat("\n * Models: \n"); print(object@modelName)
    cat("\n * Outcomes ('positive' first): \n"); print(object@outcomes)
    lengthShow <- min(10, length(object@trueOutcome))
    cat("\n * True outcome (limited to first 10): \n"); print(object@trueOutcome[1:lengthShow])
    nrowShow <- min(10, nrow(object@predOutcome))
    cat("\n * Predicted outcome (limited to first 10): \n"); print(object@predOutcome[1:nrowShow, ])
    cat("\n * Prediction probabilities (limited to first 10): \n"); print(object@predProb[1:nrowShow, ])
    cat("\n * Test set statistics: \n"); print(object@testStats)
  }
)

#' Plot method for objects of class modelPredictions
#' @import reshape2
#' @import ggplot2
#' @export
#'
setMethod(
  f = "plot",
  signature = "modelPredictions",
  definition = function(x, y, ...) {
    tmp <- data.frame(x@testStats)
    tmp$type <- rownames(tmp)
    tmp <- reshape2::melt(tmp, id = c("type"))

    ggplot2::ggplot(tmp, aes(value, variable)) +
      geom_point(alpha = 0.6, size = 5, aes(color = type, shape = type)) +
      facet_wrap( ~ type, ncol = 3, as.table = FALSE) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour="grey60", linetype="dashed"),
            legend.position = c("top"),
            text = element_text(size=11),
            legend.title = element_blank(),
            legend.key = element_blank() ) +
      xlab('Value') +
      ylab('Model')
  }
)
