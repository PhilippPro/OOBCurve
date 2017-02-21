library(mlr)
library(ranger)
library(randomForest)

context("Output check")

test_that("classification ranger", {
  num.trees = 200
  data = getTaskData(sonar.task)
  mod = ranger(Class ~., data = data, num.trees = num.trees, keep.inbag = TRUE)
  measures = list(mmce, auc, brier)
  results = OOBCurve(mod, measures = measures, task = sonar.task, data = data)
  
  expect_true(is.data.frame(results))
  expect_equal(dim(results), c(num.trees, length(measures)))
  expect_false(any(is.na(results[, 1:2])))
  expect_true(any(is.na(results[, 3])))
})

test_that("regression ranger", {
  num.trees = 200
  data = getTaskData(bh.task)
  mod = ranger(medv ~., data = data, num.trees = num.trees, keep.inbag = TRUE)
  measures = list(mse, mae)
  results = OOBCurve(mod, measures = measures, task = bh.task, data = data)

  expect_true(is.data.frame(results))
  expect_equal(dim(results), c(num.trees, length(measures)))
  expect_true(any(is.na(results[, 1])))
  expect_true(any(is.na(results[, 2])))
})

test_that("classification randomForest", {
  ntree = 200
  # Classification
  data = getTaskData(sonar.task)
  mod = randomForest(Class ~., data = data, ntree = ntree, keep.inbag = TRUE)
  # Application of the main function
  measures = list(mmce, auc, brier)
  results = OOBCurve(mod, measures = measures, task = sonar.task, data = data)
  
  expect_true(is.data.frame(results))
  expect_equal(dim(results), c(ntree, length(measures)))
  expect_false(any(is.na(results[, 1:2])))
  expect_true(any(is.na(results[, 3])))
})

test_that("regression randomForest", {
  ntree = 200
  data = getTaskData(bh.task)
  mod = randomForest(medv ~., data = data, ntree = ntree, keep.inbag = TRUE)
  measures = list(mse, mae)
  results = OOBCurve(mod, measures = measures, task = bh.task, data = data)
  
  expect_true(is.data.frame(results))
  expect_equal(dim(results), c(ntree, length(measures)))
  expect_true(any(is.na(results[, 1])))
  expect_true(any(is.na(results[, 2])))
})
