# OOBCurve

With the help of this package the out of bag learning curve for random forests 
can be created for any measure that is available in the `mlr` package. 

Supported random forest packages are `randomForest` and `ranger` and trained models of 
these packages with the `train` function of `mlr`.

Available measures can be looked up on the mlr [tutorial page](http://mlr-org.github.io/mlr-tutorial/release/html/measures/index.html).

Installation: 

```R
devtools::install_github("PhilippPro/OOBCurve")
```

Examples: 

```R
library(ranger)
library(randomForest)

# Classification
data = getTaskData(sonar.task)
mod = ranger(Class ~., data = data, num.trees = 100, keep.inbag = TRUE)

# Alternatively use randomForest
# mod = randomForest(Class ~., data = data, ntree = 100, keep.inbag = TRUE)
# Alternatively use train of mlr
# mod = train(makeLearner("classif.ranger", keep.inbag = TRUE), sonar.task)

# Application of the main function
results = OOBCurve(mod, measures = list(mmce, auc, brier), task = sonar.task, data = data)
# Plot the generated results
plot(results$mmce, type = "l", ylab = "oob-mmce", xlab = "ntrees")
plot(results$auc, type = "l", ylab = "oob-auc", xlab = "ntrees")
plot(results$brier, type = "l", ylab = "oob-brier-score", xlab = "ntrees")

# Regression
data = getTaskData(bh.task)
mod = ranger(medv ~., data = data, num.trees = 100, keep.inbag = TRUE)
# Application of the main function
results = OOBCurve(mod, measures = list(mse, mae), task = bh.task, data = data)
# Plot the generated results
plot(results$mse, type = "l", ylab = "oob-mse", xlab = "ntrees")
plot(results$mae, type = "l", ylab = "oob-mae", xlab = "ntrees")
```

