#' @title Mean Absolute Error generic function
#' 
#' @description This function calculates the mean absolute error
#' 
#' @param .prediction An object containing the predictions and the truth
#' @export
EvaluatorMAE <- function(.prediction) {
  UseMethod("EvaluatorMAE")
}

#' @title Mean Absolute Error method for data.frame
#' 
#' @description This method calculates the mean absolute error for a data.frame
#' 
#' @param .prediction A data.frame with two columns: prediction and truth
#' @examples
#' cars.data <- Dataset(data = cars, target = "dist")
#' xgb <- InducerConstructer(configuration = list(nrounds = 10, verbose = 0), method = "XGBoost")
#' model.xgb <- fit(xgb, cars.data)
#' prediction <- predict(model.xgb, newdata = cars.data[c(1, 2, 3, 4), ])
#' EvaluatorMAE(prediction)
#' @export
EvaluatorMAE.data.frame <- function(.prediction) {
  assertNames(names(.prediction), subset.of = c("prediction", "truth"))
  mean(abs(.prediction$prediction - .prediction$truth))
}

#' @title Mean Absolute Error method for ResamplePrediction
#' 
#' @description This method calculates the mean absolute error for a ResamplePrediction object
#' 
#' @param .prediction A ResamplePrediction object
#' @examples
#' cars.data <- Dataset(data = cars, target = "dist")
#' xgb <- InducerConstructer(configuration = list(nrounds = 10, verbose = 0), method = "XGBoost")
#' cv5 <- splt$cv(folds = 5)
#' rp <- resample(cars.data, xgb, cv5)
#' EvaluatorMAE(rp)
EvaluatorMAE.ResamplePrediction <- function(.prediction) {
  res <- numeric(length(.prediction))
  for (split in names(.prediction)) {
    pre <- .prediction[[split]]$predictions
    assertNames(names(pre), subset.of = c("prediction", "truth"))
    res[split] <- mean(abs(pre$prediction - pre$truth))
  }
  mean(res)
}
