test_that("test evaluation", {
  mae <- evl$mae
  rmse <- evl$rmse
  auc <- evl$auc
  acc <- evl$accuracy
  
  expect_equal(mae, EvaluatorMAE)
  expect_equal(rmse, EvaluatorRMSE)
  expect_equal(auc, EvaluatorAUC)
  expect_equal(acc, EvaluatorAccuracy)
  
  # cars.dara <- Dataset(cars, target = "speed")
  # mod1 <- InducerXGBoost(cars.data, verbose = 0, nrounds = 10)
  # expect_equal(mae(predict(mod1, newdata = cars.data[1:4, ])), 6.5)
  # expect_equal(rmse(predict(mod1, newdata = cars.data[1:4, ])), 6.978003)
  # expect_error(auc(predict(mod1, newdata = cars.data[1:4, ])))
})
  