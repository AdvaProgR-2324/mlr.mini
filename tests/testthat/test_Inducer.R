test_that("Test InducerXgboost", {
  InducerXgboost <- InducerConstructer(configuration = list(nrounds = 100, 
                                                            max_depth = 6, 
                                                            eta = 0.3),
                                       method = "Xgboost")
  
  expect_s3_class(InducerXgboost, c("InducerXgboost", "Inducer"))
  
  # Test configuration
  expect_equal(configuration(InducerXgboost), 
               list(nrounds = 100, max_depth = 6, eta = 0.3))
  
  NewInducerXgboost <- copy(InducerXgboost, new_configuration = list(nrounds = 20))
  expect_s3_class(NewInducerXgboost, c("InducerXgboost", "Inducer"))
  
  expect_equal(configuration(NewInducerXgboost), 
               list(nrounds = 20, max_depth = 6, eta = 0.3))
  
  expect_equal(configuration(copy(InducerXgboost)), 
               list(nrounds = 100, max_depth = 6, eta = 0.3))
  
  
  ## Test metainfo (Test doesn't work yet)
  # expect_equal(metainfo(cars.data)[1:6], list(
  #   name = "cars",
  #   features = c(speed = "numeric"),
  #   targets = c(dist = "numeric"),
  #   nrow = 50,
  #   type = "regression",
  #   missings = FALSE
  # ))
})