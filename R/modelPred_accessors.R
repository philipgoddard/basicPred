#' @include modelPred_class.R
NULL

#' Set the generic for accessor (getter) for outcomes
#'
#' @export
setGeneric("getOutcome",
           function(object){
             standardGeneric("getOutcome")
           })

#' @describeIn getOutcome Define the method for getting predicted outcomes
#'
#'
setMethod("getOutcome",
          signature = "modelPredictions",
          function(object){
            out <- data.frame(object@predOutcome)
            out$outcome <- object@trueOutcome
            out[, names(out)] <- lapply(out[, names(out)], as.character)
            # want a lookup table to map level code (0, 1)
            # to outcome name (yes, no)
            lookup <- c("1" = object@outcomes[1], "0" = object@outcomes[2])
            out[, names(out)] <- lapply(out[, names(out)], function(x) unname(lookup[x]))
            out[, names(out)] <- lapply(out[, names(out)], factor)
            return(out)
          })

#' Set the generic for accessor (getter) for probabilities
#'
#'
#'@export
setGeneric("getProb",
           function(object){
             standardGeneric("getProb")
           })

#' @describeIn getProb Define the method for getting predicted probabilities
#' @export
setMethod("getProb",
          signature = "modelPredictions",
          function(object){
            out <- data.frame(object@predProb)
            out$outcome <- object@trueOutcome
            out$outcome <- as.character(out$outcome)
            # want a lookup table to map level code (0, 1)
            # to outcome name (yes, no)
            lookup <- c("1" = object@outcomes[1], "0" = object@outcomes[2])
            out$outcome <- unname(lookup[out$outcome])
            out$outcome <- as.factor(out$outcome)
            return(out)
          })

#' Set the generic for accessor (getter) for statistcs (acc, sens, spec, kappa, AUC)
#' @export
setGeneric("getStats",
           function(object){
             standardGeneric("getStats")
           })

#' @describeIn getStats Define the method for getting test set statistics
#' @export
setMethod("getStats",
          signature = "modelPredictions",
          function(object){
            out <- data.frame(object@testStats)
            return(out)
          })
