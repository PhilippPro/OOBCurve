# OOBCurve

With the help of this package the out of bag learning curve for random forests 
can be created for any measure that is available in the `mlr` package. 

Supported random forest packages are `randomForest` and `ranger` and trained models of 
these packages with the `train` function of `mlr`. Available measures can be looked up on the mlr [tutorial page](http://mlr-org.github.io/mlr-tutorial/release/html/measures/index.html).

The main function is `OOBCurve` that calculates the out-of-bag curve depending on the number of trees. 
In the newest version with the `OOBCurvePars` function out-of-bag curves can also be calculated for mtry, sample.fraction and min.node.size for the ranger package.


Installation: 

```R
devtools::install_github("PhilippPro/OOBCurve")
```

Examples: 

```R
library(mlr)
library(ranger)

# Classification
data = getTaskData(sonar.task)
sonar.task = makeClassifTask(data = data, target = "Class")
lrn = makeLearner("classif.ranger", keep.inbag = TRUE, par.vals = list(num.trees = 100))
mod = train(lrn, sonar.task)

# Alternatively use ranger directly
# mod = ranger(Class ~., data = data, num.trees = 100, keep.inbag = TRUE)
# Alternatively use randomForest
# mod = randomForest(Class ~., data = data, ntree = 100, keep.inbag = TRUE)

# Application of the main function
results = OOBCurve(mod, measures = list(mmce, auc, brier), task = sonar.task, data = data)
# Plot the generated results
plot(results$mmce, type = "l", ylab = "oob-mmce", xlab = "ntrees")
plot(results$auc, type = "l", ylab = "oob-auc", xlab = "ntrees")
plot(results$brier, type = "l", ylab = "oob-brier-score", xlab = "ntrees")

# Regression
data = getTaskData(bh.task)
bh.task = makeRegrTask(data = data, target = "medv")
lrn = makeLearner("regr.ranger", keep.inbag = TRUE, par.vals = list(num.trees = 100))
mod = train(lrn, bh.task)

# Application of the main function
results = OOBCurve(mod, measures = list(mse, mae, rsq), task = bh.task, data = data)
# Plot the generated results
plot(results$mse, type = "l", ylab = "oob-mse", xlab = "ntrees")
plot(results$mae, type = "l", ylab = "oob-mae", xlab = "ntrees")
plot(results$rsq, type = "l", ylab = "oob-mae", xlab = "ntrees")

# Use OOBCurvePars for OOBCurve of other hyperparameters
library(mlr)
task = sonar.task

lrn = makeLearner("classif.ranger", predict.type = "prob", num.trees = 1000)
results = OOBCurvePars(lrn, task, measures = list(auc))
plot(results$par.vals, results$performances$auc, type = "l", xlab = "mtry", ylab = "auc")

lrn = makeLearner("classif.ranger", predict.type = "prob", num.trees = 1000, replace = FALSE)
results = OOBCurvePars(lrn, task, pars = "sample.fraction", measures = list(mmce))
plot(results$par.vals, results$performances$mmce, type = "l", xlab = "sample.fraction", ylab = "mmce")

results = OOBCurvePars(lrn, task, pars = "min.node.size", measures = list(mmce))
plot(results$par.vals, results$performances$mmce, type = "l", xlab = "min.node.size", ylab = "mmce")
```

