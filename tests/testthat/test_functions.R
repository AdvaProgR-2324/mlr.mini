
test_that("Test Dataset", {
  cars.data <- Dataset(data = cars, target = "dist")
  expect_s3_class(cars.data, c("DatasetRegression", "Dataset"))
  
  expect_equal(as.data.frame(cars.data[c(1,2,3,4), ]), 
               data.frame(dist = c(2,10,4,22), speed = c(4,4,7,7)))
  
  expect_equal(as.data.frame(cars.data[c(1, 2), "dist"]), data.frame(dist = c(2,10)))
  
  expect_error(cars.data[, "speed"])
  
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
