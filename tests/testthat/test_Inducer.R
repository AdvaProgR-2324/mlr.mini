test_that("Test InducerXgboost", {
  
  # Test constructer:
  InducerXgboost <- InducerConstructer(configuration = list(nrounds = 100, 
                                                            max_depth = 6, 
                                                            eta = 0.3),
                                       method = "Xgboost")
  
  expect_s3_class(InducerXgboost, c("InducerXgboost", "Inducer"))
  
  # Test copy:
  NewInducerXgboost <- copy(InducerXgboost, new_configuration = list(nrounds = 20))
  expect_s3_class(NewInducerXgboost, c("InducerXgboost", "Inducer"))
  
  # Test configuration:
  expect_equal(configuration(InducerXgboost), 
               list(nrounds = 100, max_depth = 6, eta = 0.3))
  expect_equal(configuration(NewInducerXgboost), 
               list(nrounds = 20, max_depth = 6, eta = 0.3))
  expect_equal(configuration(copy(InducerXgboost)), 
               list(nrounds = 100, max_depth = 6, eta = 0.3))
  configuration(InducerXgboost) <- list(nrounds = 10, max_depth = 4, verbose = 0)
  expect_equal(configuration(InducerXgboost),
               list(nrounds = 10, max_depth = 4, eta = 0.3, verbose = 0))
  
})