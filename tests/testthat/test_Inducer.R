test_that("Test Inducer", {
  
  # Load Data:
  cars.data <- Dataset(cars, target = "dist")
  
  # Test constructer:
  xgb <- InducerXGBoost(nrounds = 100, max_depth = 6, eta = 0.3)
  xgb_mod <- InducerXGBoost(cars.data, nrounds = 10, verbose = 0)
  
  rf <- InducerRandomForest(maxnodes = 6)
  rf_mod <- InducerRandomForest(cars.data, maxnodes = 6)
  expect_s3_class(rf_mod$inducer, c("InducerRandomForest", "Inducer"))
  
  expect_s3_class(xgb, c("InducerXgboost", "Inducer"))
  expect_s3_class(xgb_mod, "Model")
  
  # Test copy:
  new_xgb <- copy(xgb, new_configuration = list(nrounds = 20))
  expect_s3_class(new_xgb, c("InducerXgboost", "Inducer"))
  
  # Test configuration:
  expect_equal(configuration(xgb), 
               list(nrounds = 100, max_depth = 6, eta = 0.3))
  expect_equal(configuration(new_xgb), 
               list(nrounds = 20, max_depth = 6, eta = 0.3))
  expect_equal(configuration(copy(xgb)), 
               list(nrounds = 100, max_depth = 6, eta = 0.3))
  configuration(xgb) <- list(nrounds = 10, max_depth = 4, verbose = 0)
  expect_equal(configuration(xgb),
               list(nrounds = 10, max_depth = 4, eta = 0.3, verbose = 0))
  
})
