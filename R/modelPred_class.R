#' S4 class definition for model predictions. Only applicable for two class problems
#'
#' @slot modelName a character vector containing the names of the fitted models
#' @slot outcomes a character vector indicating the possible outcomes
#' @slot trueOutcome a numeric vector indicating the true outcome. Stored as binary 1/0 correspoinding to positive/ negative class
#' @slot predOutcome a matrix corresponding to the predictions (binary 1/0) from each model
#' @slot predProb a martix storing the predicted probabilities of the positive class
#' @slot testStats a matrix storing the sens, spec, acc, auc and kappa for each model's predictions
setClass(
  Class = "modelPredictions",
  slots = list(
    modelName = "character",
    outcomes = "character",
    trueOutcome = "numeric",
    predOutcome = "matrix",
    predProb = "matrix",
    testStats = "matrix"),

  validity = function(object) {
    ###
    ###
    # can (and should) expand on this
    ###
    ###
    cat("~~~ modelPredictions: inspector~~~ \n")
    if(length(object@modelName) != ncol(object@predProb)) {
      stop("[modelPredictions: validation] the number of predictions is wrong")
    } else {

    }
    return(TRUE)
  }
)

#' Initializer for modelPredictions class
#'
#' @export
#'
setMethod(
  f = "initialize",
  signature = "modelPredictions",
  definition = function(.Object, modelName, outcomes, trueOutcome, predOutcome, predProb, testStats){
    cat("*** modelPredictions: initialiser *** \n")

    colnames(predOutcome) <- modelName
    rownames(predOutcome) <- paste0("C", 1:nrow(predOutcome), sep = "")
    colnames(predProb) <- modelName
    rownames(predProb) <- paste0("P", 1:nrow(predProb), sep = "")
    colnames(testStats) <- modelName
    rownames(testStats) <- c("Acc", "Sens", "Spec", "Kappa", "AUC")

    .Object@modelName <- modelName
    .Object@outcomes <- outcomes
    .Object@trueOutcome <- trueOutcome
    .Object@predOutcome <- predOutcome
    .Object@predProb <- predProb
    .Object@testStats <- testStats
    return(.Object)
  }
)
