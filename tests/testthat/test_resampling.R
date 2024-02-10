test_that("test resampling", {
  cars.data <- Dataset(data = cars, target = "dist")
  
  cv5 <- splt$cv(folds = 5)
  
  expect_s3_class(cv5, "SplitCV")
  expect_s3_class(cv5, "Split")
  
  cars.split <- cv5(cars.data)
  
  expect_s3_class(cars.split, "SplitInstanceCV")
  expect_s3_class(cars.split, "SplitInstance")
  
  expect_equal(length(cars.split), 5)
  
})