#' @title Out of Bag Learning curve
#'
#' @description
#' With the help of this function the out of bag learning curve for random forests 
#' can be created for any measure that is available in the mlr package.  
#'
#' @param mod
#'   An object of class randomForest, as that created by the function randomForest with option keep.inbag = TRUE
#' @param measures
#'   List of performance measure(s) of mlr to evaluate. Default is auc only.
#' @param task
#'   Learning task
#' @return
#'   Returns a dataframe with a column for each desired measure
#' @export
#' @examples
#' library(mlr)
#' library(randomForest)
#' data = getTaskData(sonar.task)
#' mod = randomForest(Class ~., data = data, ntree = 100, keep.inbag = TRUE)
#' results = OOBCurve(mod, measures = list(mmce, auc, brier), task = sonar.task)
#' # Plot the generated results
#' plot(results$mmce, type = "l", ylab = "oob-mmce", xlab = "ntrees")
#' plot(results$auc, type = "l", ylab = "oob-auc", xlab = "ntrees")
#' plot(results$brier, type = "l", ylab = "oob-brier-score", xlab = "ntrees")
#' 
OOBCurve = function(mod, measures = list(auc), task) {
  truth = mod$y
  preds = predict(mod, newdata = data, predict.all = TRUE)
  inbag = mod$inbag
  ntree = ncol(preds$individual)
  nobs = nrow(preds$individual)
  num_levels = nlevels(preds$aggr)
  pred_levels = levels(preds$aggr)
  prob_array = array(data = NA, dim = c(nobs, ntree, num_levels), dimnames = list(NULL, NULL, pred_levels))
  for(i in 1:length(pred_levels)) {
    predis = (preds$individual == pred_levels[i]) * 1
    predis = predis * ((inbag == 0) * 1) # only use observations that are out of bag
    predis = rowCumsums(predis)
    prob_array[, , i] = predis * (1 / rowCumsums((inbag == 0) * 1)) # divide by the number of observations that are out of bag
    #prob_array[, , i] = predis %*% diag(1/(1:ntree))
  }
  result = data.frame(t(apply(prob_array, 2, function(x) calculateMlrMeasure(x, measures))))
  return(result)
}

calculateMlrMeasure = function(x, measures) {
  mlrpred = mlr::makePrediction(task.desc = taskmlr$task.desc, row.names = names(truth), id = names(truth), truth = truth,
    predict.type = "prob", predict.threshold = 0.5, y = x, time = NA)
  performance(mlrpred, measures)
}
