#' @include modelPred_class.R
NULL

#' Constructor function for objects of class modelPrediction. Always assume first class is class of interest.
#' @import caret
#' @import pROC
#' @export
modelPredConstr <- function(models, inputData, trueResult, classLevels){
  # add a test for inputs, especially class levels

  modelNames <- names(models)

  classPred <- Map(function(x, y) predict(x, newdata = y), models, inputData)
  classPred <- do.call(cbind.data.frame, classPred)
  classProb <- Map(function(x, y) predict(x, newdata = y, type = "prob")[1], models, inputData)
  classProb <- do.call(cbind.data.frame, classProb)

  # calling this conf in and out of loop is confusing
  conf <- lapply(classPred, function(x) {
    conf <- caret::confusionMatrix(x, trueResult)
    out <- list(Acc = conf$overall["Accuracy"],
                Sens = conf$byClass["Sensitivity"],
                Spec = conf$byClass["Specificity"],
                Kappa = conf$overall["Kappa"])
    out
  })

  # sort this- calculate ROC AUC then just rbind to the matrix with the four other stats

  AUCROC <- lapply(classProb, function(x) {
    rocObj <- pROC::roc(response = trueResult,
                        predictor = x,
                        # assumes second class is event of interest, so reverse
                        levels = rev(levels(trueResult)))
    auc(rocObj)[1]
  })

  testSetStats <- as.matrix(do.call(rbind, conf))
  testSetStats <- cbind(testSetStats, unname(unlist(AUCROC)))
  testSetStats <- unname(t(testSetStats))

  # assume first class = 1 = success class
  levels(trueResult)[levels(trueResult)==classLevels[1]] <- "1"
  levels(trueResult)[levels(trueResult)==classLevels[2]] <- "0"
  trueResult <- as.numeric(as.character(trueResult))

  classPred[, names(classPred)] <- lapply(classPred[, names(classPred)], function(x){
    levels(x)[levels(x)==classLevels[1]] <- "1"
    levels(x)[levels(x)==classLevels[2]] <- "0"
    x <- as.numeric(as.character(x))
  })
  classPred <- unname(as.matrix(classPred))

  classProb <- Map(function(x, y) predict(x, newdata = y, type = "prob")[, 1], models, inputData)
  classProb <- do.call(cbind.data.frame, classProb)
  classProb <- unname(as.matrix(classProb))

  cat("~~~ modelPredictions: constructor ~~~\n")
  new(Class = "modelPredictions",
      modelName = modelNames,
      outcomes = classLevels,
      trueOutcome = trueResult,
      predOutcome = classPred,
      predProb = classProb,
      testStats = testSetStats)
}
