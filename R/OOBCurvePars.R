#' @title OOBCurvePars
#' 
#' @description
#' With the help of this function the out of bag curves for parameters like mtry, sample.fraction and 
#' min.node.size of random forests can be created for any measure that is available in the mlr package. 
#'
#' @param lrn
#'   The learner created with \code{\link[mlr]{makeLearner}}. Currently only \code{\link[ranger]{ranger}} is supported. 
#'   num.trees has to be set sufficiently high to produce smooth curves.  
#' @param task
#'   Learning task created by the function \code{\link[mlr]{makeClassifTask}} or \code{\link[mlr]{makeRegrTask}} of \href{https://github.com/mlr-org/mlr}{mlr}. 
#' @param pars
#'   One of the hyperparameter "mtry", "sample.fraction" or "min.node.size".
#' @param nr.grid
#'   Number of points on hyperparameter space that should be evaluated (distributed equally)
#' @param par.vals
#'   Optional vector of hyperparameter points that should be evaluated. If set, nr.grid is not used anymore. Default is NULL.
#' @param measures 
#'   List of performance measure(s) of mlr to evaluate. Default is mmce for classification and mse for regression.
#'   See the \href{http://mlr-org.github.io/mlr-tutorial/release/html/measures/index.html}{mlr tutorial} for a list of available measures 
#'   for the corresponding task.
#' @return 
#'   Returns a list with parameter values and a list of performances.
#' @export
#' @seealso \code{\link{OOBCurve}} for out-of-bag curves dependent on the number of trees.
#' @examples
#' \dontrun{
#' library(mlr)
#' task = sonar.task
#' 
#' lrn = makeLearner("classif.ranger", predict.type = "prob", num.trees = 1000)
#' results = OOBCurvePars(lrn, task, measures = list(auc))
#' plot(results$par.vals, results$performances$auc, type = "l", xlab = "mtry", ylab = "auc")
#' 
#' lrn = makeLearner("classif.ranger", predict.type = "prob", num.trees = 1000, replace = FALSE)
#' results = OOBCurvePars(lrn, task, pars = "sample.fraction", measures = list(mmce))
#' plot(results$par.vals, results$performances$mmce, type = "l", xlab = "sample.fract.", ylab = "mmce")
#' 
#' results = OOBCurvePars(lrn, task, pars = "min.node.size", measures = list(mmce))
#' plot(results$par.vals, results$performances$mmce, type = "l", xlab = "min.node.size", ylab = "mmce")}
OOBCurvePars = function(lrn, task, pars = c("mtry"), nr.grid = 10, par.vals = NULL, measures = list(auc)) {
  if (is.null(par.vals)) {
  if (pars == "mtry") {
    nfeats = mlr::getTaskNFeats(task)
    par.vals = round(seq(1, nfeats, length.out = nr.grid))
  } 
  if (pars == "sample.fraction") {
    par.vals = seq(0.05, 1, length.out = nr.grid)
  }
  if (pars == "min.node.size") {
    n = mlr::getTaskSize(task)
    nodesize_max = trafo_nodesize_end(1, n)
    par.vals = round(seq(1, nodesize_max, length.out = nr.grid))
  }
  par.vals = unique(par.vals)    
  }
  
  performances = list()
  for(i in seq_along(par.vals)) {
    print(paste0("Iteration ", i, " of ", length(par.vals), ": Training parameter ", pars, "=", round(par.vals[i], 3)))
    par.vals.i = list(par.vals[i])
    names(par.vals.i) = pars
    lrn = mlr::setHyperPars(lrn, par.vals = par.vals.i)
    mod = mlr::train(lrn, task)
    oob = mlr::getOOBPreds(mod, task)
    performances[[i]] = mlr::performance(oob, measures = measures)
  }
  performances = data.frame(do.call(rbind, performances))
  rownames(performances) = paste0(pars, "=", round(par.vals, 3))
  
  list(par.vals = par.vals, performances = performances)
}

trafo_nodesize_end = function(x, size) ceiling(2^(log(size * 0.2, 2) * x))

