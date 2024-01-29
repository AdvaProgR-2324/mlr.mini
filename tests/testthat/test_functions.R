
test_that("Test Dataset", {
  cars.data <- Dataset(data = cars, target = "dist")
  expect_equal(class(cars.data), c("DatasetRegression", "Dataset"))
})
