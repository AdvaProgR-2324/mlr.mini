test_that("Test Dataset", {
  cars.data <- Dataset(data = cars, target = "dist")
  expect_s3_class(cars.data, c("DatasetRegression", "Dataset"))
  
  iris.data <- Dataset(iris, target = "Species")
  expect_s3_class(iris.data, c("DatasetClassification", "Dataset"))
  
  expect_s3_class(Dataset(iris, target = "Sepal.Length"),
                  c("DatasetRegression", "Dataset"))
  expect_s3_class(Dataset(iris, target = "Species", type = "regression"),
                  c("DatasetRegression", "Dataset"))
  expect_s3_class(Dataset(iris, target = "Sepal.Length", type = "classification"),
                  c("DatasetClassification", "Dataset"))
  
  expect_error(Dataset(cars, target = "dist", type = "Classification"))
  expect_error(Dataset(cars, target = "cars"))
  expect_error(Dataset(cars, target = "dist", type = "regression", name = 1))
  expect_error(Dataset(cars))
  expect_error(Dataset(as.matrix(cars), target = "dist"))
  
  expect_equal(cars.data$name, "cars")
  expect_equal(cars.data$target, "dist")
  expect_equal(cars.data$type, "regression")
  expect_equal(cars.data$data, cars[, c("dist", "speed")])
  expect_equal(iris.data$name, "iris")
  expect_equal(iris.data$target, "Species")
  expect_equal(iris.data$type, "classification")
  expect_equal(iris.data$data,
               iris[, c("Species", "Sepal.Length",
                        "Sepal.Width", "Petal.Length", "Petal.Width")])
  
  expect_equal(Dataset(cars, target = "dist", name = "cars2")$name, "cars2")
  
  expect_equal(as.data.frame(iris.data, columns = "target"), 
               data.frame(Species = iris$Species))
  expect_equal(as.data.frame(iris.data, columns = "features"),
               data.frame(Sepal.Length = iris$Sepal.Length,
                          Sepal.Width = iris$Sepal.Width,
                          Petal.Length = iris$Petal.Length,
                          Petal.Width = iris$Petal.Width))
  
  expect_equal(as.data.frame(cars.data[c(1,2,3,4), ]), 
               data.frame(dist = c(2,10,4,22), speed = c(4,4,7,7)))
  
  expect_equal(as.data.frame(cars.data[c(1, 2), "dist"]),
               data.frame(dist = c(2,10)))
  
  expect_error(cars.data[, "speed"])
  
  expect_equal(metainfo(cars.data)[1:6], list(
    name = "cars",
    features = c(speed = "num"),
    targets = c(dist = "num"),
    nrow = 50,
    type = "regression",
    missings = FALSE
  ))
  
  expect_equal(metainfo(iris.data)[1:6], list(
    name = "iris",
    features = c(Sepal.Length = "num", Sepal.Width = "num",
                 Petal.Length = "num", Petal.Width = "num"),
    targets = c(Species = "fact"),
    nrow = 150,
    type = "classification",
    missings = FALSE
  ))
  
  expect_equal(metainfo(Dataset(data = data.frame(a = c(1, NA, 3), b = c(NA, 2, 3)), target = "a"))$missings, TRUE)
  expect_equal(metainfo(cars.data[, "dist"])$features, NULL)
  
})
