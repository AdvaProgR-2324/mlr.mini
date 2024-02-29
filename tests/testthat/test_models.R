test_that("test models", {
  set.seed(123)
  cars.data <- Dataset(data = cars, target = "dist")
  xgb <-
    InducerConstructer(configuration = list(nrounds = 10, verbose = 0),
                       method = "XGBoost")
  model.xgb <- fit(xgb, cars.data)
  expect_s3_class(model.xgb, "Model")
  expect_s3_class(model.xgb, "ModelRegression")
  expect_s3_class(model.xgb, "ModelXGBoost")
  
  expect_equal(inducer(model.xgb), xgb)
  expect_equal(configuration(model.xgb), list(nrounds = 10, verbose = 0))
  expect_equal(configuration(model.xgb), configuration(xgb))
  expect_named(modelInfo(model.xgb), "training.time.sec")
  expect_numeric(modelInfo(model.xgb)$training.time.sec)
  
  expect_equal(model.xgb$model$feature_names, c("speed"))
  
  expect_equal(round(predict(model.xgb, newdata = data.frame(speed = 10)), 5),
               23.76096)
  expect_equal(predict(model.xgb, newdata = cars.data[c(1, 2, 3, 4), ]),
               data.frame(
                 prediction = c(5.710046, 5.710046, 12.451396, 12.451396),
                 truth = c(2, 10, 4, 22)
               ))
  
  # doesn't work yet
  # expect_equal(
  #   modelObject(model.xgb),
  #   xgboost::xgboost(
  #     data = as.matrix(cars.data$data["speed"]),
  #     label = cars.data$data[[cars.data$target]],
  #     nrounds = 10,
  #     verbose = 0
  #   )
  # )
  
  iris.data <- Dataset(iris[1:130, ], target = "Sepal.Length")
  xgb.iris <- InducerConstructer(method = "XGBoost", configuration = list(nrounds = 20))
  model <- fit(xgb.iris, iris.data)
  expect_s3_class(model, "Model")
  expect_s3_class(model, "ModelRegression")
  expect_s3_class(model, "ModelXGBoost")
  
  expect_equal(inducer(model), xgb.iris)
  expect_equal(configuration(model), list(nrounds = 20))
  expect_equal(configuration(model), configuration(xgb.iris))
  expect_named(modelInfo(model), "training.time.sec")
  expect_numeric(modelInfo(model)$training.time.sec)
  
  result_expected <- 
    c(7.320305, 7.335830, 6.260023, 6.144281, 6.467174, 7.148435, 6.195793,
      6.681785, 6.184646, 6.695359, 6.355690, 6.328278, 5.936321, 7.015198,
      6.240932, 6.132180, 5.785251, 6.422567, 6.423667, 6.228498)
  
  expect_equal(round(predict(model, newdata = iris[131:150,]), 6), result_expected)
  
  expect_equal(round(predict(model, newdata = Dataset(iris[131:150, ], target = "Sepal.Length")), 6),
               data.frame(
                 prediction = result_expected,
                 truth = iris[131:150, "Sepal.Length"]))

  # predict(model, newdata = data.frame(Sepal.Width = 3, Petal.Length = 5, Petal.Width = 1.5, Species = "setosa"))
  
  ## Random Forest
  rf.cars <- InducerConstructer(method = "RandomForest", configuration = list(ntree = 10))
  model <- fit(rf.cars, cars.data)
  expect_s3_class(model, "Model")
  expect_s3_class(model, "ModelRegression")
  expect_s3_class(model, "ModelRandomForest")
  
  expect_equal(inducer(model), rf.cars)
  expect_equal(configuration(model), list(ntree = 10))
  expect_equal(configuration(model), configuration(rf.cars))
  expect_named(modelInfo(model), "training.time.sec")
  expect_numeric(modelInfo(model)$training.time.sec)
  
  expect_equal(predict(model, newdata = data.frame(speed = 10)), 23.76)
  # rownames have diffrent modes?
  # expect_equal(predict(model, newdata = cars.data[c(1:4), ]),
  #              data.frame(
  #                prediction = c(9.91, 9.91, 15.09, 15.09),
  #                truth = c(2, 10, 4, 22)
  #              ))
  
  rf.iris <- InducerConstructer(method = "RandomForest", configuration = list(ntree = 20))
  model <- fit(rf.iris, iris.data)
  expect_s3_class(model, "Model")
  expect_s3_class(model, "ModelRegression")
  expect_s3_class(model, "ModelRandomForest")
  
  expect_equal(inducer(model), rf.iris)
  expect_equal(configuration(model), list(ntree = 20))
  expect_equal(configuration(model), configuration(rf.iris))
  expect_named(modelInfo(model), "training.time.sec")
  expect_numeric(modelInfo(model)$training.time.sec)
  
  expect_equal(round(predict(model, newdata = iris[145:150,]), 6),
               c(6.618186, 6.404991, 6.026440, 6.376491, 6.670650, 6.339716))
  # rownames have diffrent modes
  # expect_equal(
  #  round(predict(model, newdata = Dataset(iris[145:150, ], target = "Sepal.Length")), 6),
  #   data.frame(
  #     prediction = c(6.618186, 6.404991, 6.026440, 6.376491, 6.670650, 6.339716),
  #     truth = iris[145:150, "Sepal.Length"]
  #   )
  # )
  
  # doesn't work yet
  # expect_equal(predict(model, newdata = data.frame(Sepal.Width = 3.3, Petal.Length = 5.7, Petal.Width = 2.5, Species = "virginica")),
  #              6.618186)
  
  iris.classif <- Dataset(iris, target = "Species")
  rf.classif <- InducerConstructer(method = "RandomForest", configuration = list(ntree = 30))
  model <- fit(rf.classif, iris.classif)
  expect_s3_class(model, "Model")
  expect_s3_class(model, "ModelClassification")
  expect_s3_class(model, "ModelRandomForest")
  
  expect_equal(inducer(model), rf.classif)
  expect_equal(configuration(model), list(ntree = 30))
  expect_equal(configuration(model), configuration(rf.classif))
  expect_named(modelInfo(model), "training.time.sec")
  expect_numeric(modelInfo(model)$training.time.sec)
  
  expect_equal(predict(model, newdata = iris[1:4, ]), factor(
    c("setosa", "setosa", "setosa", "setosa"),
    levels = c("setosa", "versicolor", "virginica")
  ))
  # rownames have diffrent modes
  # expect_equal(predict(model, newdata = iris.classif[1:4, ]), data.frame(
  #   prediction = factor(
  #     c("setosa", "setosa", "setosa", "setosa"),
  #     levels = c("setosa", "versicolor", "virginica")
  #   ),
  #   truth = factor(
  #     c("setosa", "setosa", "setosa", "setosa"),
  #     levels = c("setosa", "versicolor", "virginica")
  #   )
  # ))
})
