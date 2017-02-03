# OOBCurve

With the help of this package the out of bag learning curve for random forests 
can be created for any measure that is available in the mlr package.

Installation: 

    ```splus
    devtools::install_github("PhilippPro/OOBCurve")
    ```

Examples: 

    ```splus
    library(mlr)
    library(randomForest)
 
    # Classification
    data = getTaskData(sonar.task)
    mod = randomForest(Class ~., data = data, ntree = 100, keep.inbag = TRUE)
    results = OOBCurve(mod, measures = list(mmce, auc, brier), task = sonar.task)
    # Plot the generated results
    plot(results$mmce, type = "l", ylab = "oob-mmce", xlab = "ntrees")
    plot(results$auc, type = "l", ylab = "oob-auc", xlab = "ntrees")
    plot(results$brier, type = "l", ylab = "oob-brier-score", xlab = "ntrees")
 
    # Regression
    data = getTaskData(bh.task)
    mod = randomForest(medv ~., data = data, ntree = 100, keep.inbag = TRUE)
    results = OOBCurve(mod, measures = list(mse, mae), task = bh.task)
    # Plot the generated results
    plot(results$mse, type = "l", ylab = "oob-mse", xlab = "ntrees")
    plot(results$mae, type = "l", ylab = "oob-mae", xlab = "ntrees")
    ```

