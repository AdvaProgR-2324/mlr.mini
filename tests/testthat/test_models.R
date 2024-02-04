test_that("test models", {
  cars.data <- Dataset(data = cars, target = "dist")
  xgb <-
    InducerConstructer(configuration = list(nrounds = 10, verbose = 0),
                       method = "XGBoost")
  model.xgb <- fit(xgb, cars.data)
  expect_s3_class(model.xgb, "Model")
  expect_s3_class(model.xgb, "ModelRegression")
  expect_s3_class(model.xgb, "ModelXGBoost")
  
  expect_equal(model.xgb$model$feature_names, c("speed"))
  
  expect_equal(round(predict(model.xgb, newdata = data.frame(speed = 10)), 5),
               23.76096)
  expect_equal(predict(model.xgb, newdata = cars.data[c(1, 2, 3, 4), ]),
               data.frame(
                 prediction = c(5.710046, 5.710046, 12.451396, 12.451396),
                 truth = c(2, 10, 4, 22)
               ))
  
  expect_equal(configuration(model.xgb), list(nrounds = 10, verbose = 0))
  
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
})
