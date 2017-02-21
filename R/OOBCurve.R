#' @title Out of Bag Learning curve
#'
#' @description
#' With the help of this function the out of bag learning curve for random forests 
#' can be created for any measure that is available in the mlr package.  
#'
#' @param mod
#'   An object of class randomForest or ranger, as that created by the function randomForest/ranger with option keep.inbag = TRUE
#' @param measures
#'   List of performance measure(s) of mlr to evaluate. Default is auc only.
#'   See the \href{http://mlr-org.github.io/mlr-tutorial/release/html/measures/index.html}{mlr tutorial} for a list of available measures 
#'   for the corresponding task.
#' @param task
#'   Learning task created by the function \code{\link[mlr]{makeClassifTask}} or \code{\link[mlr]{makeRegrTask}} of \href{https://github.com/mlr-org/mlr}{mlr}. 
#' @param data
#'   Original data that was used for training the random forest. 
#' @return
#'   Returns a dataframe with a column for each desired measure.
#' @export
#' @importFrom mlr auc
#' @importFrom stats predict
#' @examples
#' library(ranger)
#' library(randomForest)
#' 
#' # Classification
#' data = getTaskData(sonar.task)
#' mod = ranger(Class ~., data = data, num.trees = 100, keep.inbag = TRUE)
#' # Alternatively use randomForest
#' # mod = randomForest(Class ~., data = data, ntree = 100, keep.inbag = TRUE)
#' # Application of the main function
#' results = OOBCurve(mod, measures = list(mmce, auc, brier), task = sonar.task, data = data)
#' # Plot the generated results
#' plot(results$mmce, type = "l", ylab = "oob-mmce", xlab = "ntrees")
#' plot(results$auc, type = "l", ylab = "oob-auc", xlab = "ntrees")
#' plot(results$brier, type = "l", ylab = "oob-brier-score", xlab = "ntrees")
#' 
#' # Regression
#' data = getTaskData(bh.task)
#' mod = ranger(medv ~., data = data, num.trees = 100, keep.inbag = TRUE)
#' # Alternatively use randomForest
#' # mod = randomForest(medv ~., data = data, ntree = 100, keep.inbag = TRUE)
#' # Application of the main function
#' results = OOBCurve(mod, measures = list(mse, mae), task = bh.task, data = data)
#' # Plot the generated results
#' plot(results$mse, type = "l", ylab = "oob-mse", xlab = "ntrees")
#' plot(results$mae, type = "l", ylab = "oob-mae", xlab = "ntrees")
#' 
OOBCurve = function(mod, measures = list(auc), task, data) {
  UseMethod("OOBCurve")
}

#' @export
OOBCurve.randomForest.formula = function(mod, measures = list(auc), task, data) {
  tasktype = mlr::getTaskType(task)
  truth = mod$y
  preds = predict(mod, newdata = data, predict.all = TRUE)
  inbag = mod$inbag
  
  if (tasktype == "classif") {
    ntree = ncol(preds$individual)
    nobs = nrow(preds$individual)
    num_levels = nlevels(preds$aggr)
    pred_levels = levels(preds$aggr)
    prob_array = array(data = NA, dim = c(nobs, ntree, num_levels), dimnames = list(NULL, NULL, pred_levels))
    for(i in 1:length(pred_levels)) {
      predis = (preds$individual == pred_levels[i]) * 1
      predis = predis * ((inbag == 0) * 1) # only use observations that are out of bag
      prob_array[, , i] = rowCumsums(predis) * (1 / rowCumsums((inbag == 0) * 1)) # divide by the number of observations that are out of bag
    }
    result = data.frame(t(apply(prob_array, 2, function(x) calculateMlrMeasure(x, measures, task, truth, predict.type = "prob"))))
  }
  
  if (tasktype == "regr") {
    preds$individual = preds$individual * ((inbag == 0) * 1) # only use observations that are out of bag
    predis = rowCumsums(preds$individual) * (1 / rowCumsums((inbag == 0) * 1))
    result = data.frame(t(apply(predis, 2, function(x) calculateMlrMeasure(x, measures, task, truth, predict.type = "response"))))
  }
  return(result)
}

#' @export
OOBCurve.ranger = function(mod, measures = list(auc), task, data) {
  tasktype = mlr::getTaskType(task)
  truth = mlr::getTaskTargets(task)
  preds = predict(mod, data = data, predict.all = TRUE)
  inbag = do.call(cbind, mod$inbag.counts)
  
  if (tasktype == "classif") {
    ntree = mod$num.trees
    nobs = length(mod$predictions)
    num_levels = nlevels(truth)
    pred_levels = levels(truth)
    prob_array = array(data = NA, dim = c(nobs, ntree, num_levels), dimnames = list(NULL, NULL, pred_levels))
    for(i in 1:length(pred_levels)) {
      predis = (preds$predictions == i) * 1
      predis = predis * ((inbag == 0) * 1) # only use observations that are out of bag
      prob_array[, , i] = rowCumsums(predis) * (1 / rowCumsums((inbag == 0) * 1)) # divide by the number of observations that are out of bag
    }
    result = data.frame(t(apply(prob_array, 2, function(x) calculateMlrMeasure(x, measures, task, truth, predict.type = "prob"))))
  }
  if (tasktype == "regr") {
    preds$predictions = preds$predictions * ((inbag == 0) * 1) # only use observations that are out of bag
    predis = rowCumsums(preds$predictions) * (1 / rowCumsums((inbag == 0) * 1))
    result = data.frame(t(apply(predis, 2, function(x) calculateMlrMeasure(x, measures, task, truth, predict.type = "response"))))
  }
  return(result)
}

calculateMlrMeasure = function(x, measures, task, truth, predict.type) {
  mlrpred = mlr::makePrediction(task.desc = task$task.desc, row.names = names(truth), id = names(truth), truth = truth,
    predict.type = predict.type, predict.threshold = NULL, y = x, time = NA)
  mlr::performance(mlrpred, measures)
}

rowCumsums = function(x) {
  t(apply(x, 1, cumsum))
}
