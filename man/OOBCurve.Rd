\name{OOBCurve}
\alias{OOBCurve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
OOBCurve(preds, truth, measures, inbag, taskmlr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{preds}{
%%     ~~Describe \code{preds} here~~
}
  \item{truth}{
%%     ~~Describe \code{truth} here~~
}
  \item{measures}{
%%     ~~Describe \code{measures} here~~
}
  \item{inbag}{
%%     ~~Describe \code{inbag} here~~
}
  \item{taskmlr}{
%%     ~~Describe \code{taskmlr} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (preds, truth, measures, inbag, taskmlr) 
{
    ntree = ncol(preds$individual)
    nobs = nrow(preds$individual)
    num_levels = nlevels(preds$aggr)
    pred_levels = levels(preds$aggr)
    prob_array = array(data = NA, dim = c(nobs, ntree, num_levels), 
        dimnames = list(NULL, NULL, pred_levels))
    for (i in 1:length(pred_levels)) {
        predis = (preds$individual == pred_levels[i]) * 1
        predis = predis * ((inbag == 0) * 1)
        predis = rowCumsums(predis)
        prob_array[, , i] = predis * (1/rowCumsums((inbag == 
            0) * 1))
    }
    calculateMlrMeasure = function(x, measures) {
        mlrpred = makePrediction(task.desc = taskmlr$mlr.task$task.desc, 
            row.names = names(truth), id = names(truth), truth = truth, 
            predict.type = "prob", predict.threshold = 0.5, y = x, 
            time = NA)
        performance(mlrpred, measures)
    }
    result = apply(prob_array, 2, function(x) calculateMlrMeasure(x, 
        measures))
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
