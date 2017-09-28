#' @title OOBCurvePars
#' 
#' @description
#' With the help of this function the out of bag curves for parameters like mtry, sample.fraction and 
#' min.node.size of random forests can be created for any measure that is available in the mlr package. 
#'
#' @param lrn
#'   The learner created with \code{\link[mlr]{makeLearner}}. Currently only \code{\link[ranger]{ranger}} is supported. 
#'   Ntree has to be set sufficiently high to produce smooth curves.  
#' @param task
#'   Learning task created by the function \code{\link[mlr]{makeClassifTask}} or \code{\link[mlr]{makeRegrTask}} of \href{https://github.com/mlr-org/mlr}{mlr}. 
#' @param pars
#'   One of the hyperparameter "mtry", "sample.fraction" or "min.node.size".
#' @param nr.grid
#'   Number of points on hyperparameter space that should be evaluated (distributed equally)
#' @param par.vals
#'   Optional vector of hyperparameter points that should be evaluated. If set, nr.grid is not used anymore. Default is NULL.
#' @param measures 
#'   List of performance measure(s) of mlr to evaluate. Default is auc only.
#'   See the \href{http://mlr-org.github.io/mlr-tutorial/release/html/measures/index.html}{mlr tutorial} for a list of available measures 
#'   for the corresponding task.
#' @return 
#'   Returns a list with parameter values and a list of performances.
#' @export
#'
#' @examples
#' library(mlr)
#' lrn = makeLearner("classif.ranger", predict.type = "prob", num.trees = 1000)
#' task = sonar.task
#' perfs = OOBCurvePars(lrn, task)
#' plot(perfs$par.vals, unlist(perfs$perfs), type = "l", xlab = "mtry", ylab = "mmce")
#' 
#' lrn = makeLearner("classif.ranger", predict.type = "prob", num.trees = 1000, replace = FALSE)
#' perfs = OOBCurvePars(lrn, task, pars = "sample.fraction")
#' plot(perfs$par.vals, unlist(perfs$perfs), type = "l", xlab = "sample.fraction", ylab = "mmce")
#'
#' perfs = OOBCurvePars(lrn, task, pars = "min.node.size")
#' plot(perfs$par.vals, unlist(perfs$perfs), type = "l", xlab = "min.node.size", ylab = "mmce")
OOBCurvePars = function(lrn, task, pars = c("mtry"), nr.grid = 10, par.vals = NULL, measures) {
  if (is.null(par.vals)) {
  if(pars == "mtry") {
    nfeats = mlr::getTaskNFeats(task)
    par.vals = round(seq(1, nfeats, length.out = 10))
  } 
  if(pars == "sample.fraction") {
    par.vals = seq(0.05, 1, length.out = 10)
  }
  if(pars == "min.node.size") {
    n = mlr::getTaskSize(task)
    nodesize_max = trafo_nodesize_end(1, n)
    par.vals = round(seq(1, nodesize_max, length.out = 10))
  }
  par.vals = unique(par.vals)    
  }
  
  perfs = list()
  for(i in seq_along(par.vals)) {
    print(paste0("Iteration ", i, " of ", length(par.vals), ": Training parameter ", pars, "=", round(par.vals[i], 3)))
    par.vals.i = list(par.vals[i])
    names(par.vals.i) = pars
    lrn = mlr::setHyperPars(lrn, par.vals = par.vals.i)
    mod = mlr::train(lrn, task)
    oob = mlr::getOOBPreds(mod, task)
    perfs[[i]] = mlr::performance(oob)
  }
  
  list(par.vals = par.vals, perfs = perfs)
}

trafo_nodesize_end = function(x, size) ceiling(2^(log(size * 0.2, 2) * x))




