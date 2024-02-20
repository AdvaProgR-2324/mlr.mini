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
  result <- mean(abs(.prediction$prediction - .prediction$truth))
  result
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
  result <- mean(res)  # Assuming res is calculated as shown
  
  # Create a result object with a class
  result_obj <- list(value = result)
  class(result_obj) <- "EvaluatorMAE"
  return(result_obj)
}

#' @title 'print' method for `EvaluatorMAE`
#' 
#' @description This method prints the evaluator name and configuration
#' 
#' @param x An object of class `EvaluatorMAE`
#' @param ... Additional arguments
#' @export
print.EvaluatorMAE <- function(x, ...) {
  cat("Evaluator: Mean Absolute Error\n")
  cat("Configuration: ()\n")
}

#' @title Root Mean Squared Error generic function
#'
#' @description This function calculates the root mean squared error.
#'
#' @param .prediction An object containing the predictions and the truth.
#' @export
EvaluatorRMSE <- function(.prediction) {
  UseMethod("EvaluatorRMSE")
}

#' @title RMSE method for data.frame
#'
#' @description Calculates RMSE for a data.frame.
#'
#' @param .prediction A data.frame with two columns: prediction and truth.
#' @export
EvaluatorRMSE.data.frame <- function(.prediction) {
  assertNames(names(.prediction), subset.of = c("prediction", "truth"))
  result <- sqrt(mean((.prediction$prediction - .prediction$truth)^2))
  
  # Create a result object with a class
  result_obj <- list(value = result)
  class(result_obj) <- "EvaluatorRMSE"
  return(result_obj)
}

#' @title RMSE method for ResamplePrediction
#'
#' @description Calculates RMSE for a ResamplePrediction object.
#'
#' @param .prediction A ResamplePrediction object.
#' @export
EvaluatorRMSE.ResamplePrediction <- function(.prediction) {
  res <- numeric(length(.prediction))
  for (split in names(.prediction)) {
    pre <- .prediction[[split]]$predictions
    assertNames(names(pre), subset.of = c("prediction", "truth"))
    res[split] <- sqrt(mean((pre$prediction - pre$truth)^2))
  }
  result <- mean(res)  # Assuming res is calculated as shown
  
  # Create a result object with a class
  result_obj <- list(value = result)
  class(result_obj) <- "EvaluatorRMSE"
  return(result_obj)
}

#' @title 'print' method for `EvaluatorRMSE`
#' 
#' @description This method prints the evaluator name and configuration
#' @param x An object of class `EvaluatorRMSE`
#' @param ... Additional arguments
#' @export
print.EvaluatorRMSE <- function(x, ...) {
  cat("Evaluator: Root Mean Squared Error\n")
  cat("Configuration: ()\n")
  cat("Value:", x$value, "\n")
}


#' @title Accuracy generic function
#'
#' @description This function calculates accuracy.
#'
#' @param .prediction An object containing the predictions and the truth.
#' @export
EvaluatorAccuracy <- function(.prediction) {
  UseMethod("EvaluatorAccuracy")
}

#' @title Accuracy method for data.frame
#'
#' @description Calculates Accuracy for a data.frame.
#'
#' @param .prediction A data.frame where prediction column is the predicted class, and truth column is the actual class.
#' @export
EvaluatorAccuracy.data.frame <- function(.prediction) {
  assertNames(names(.prediction), subset.of = c("prediction", "truth"))
  result <- mean(.prediction$prediction == .prediction$truth)
  
  # Create a result object with a class
  result_obj <- list(value = result)
  class(result_obj) <- "EvaluatorAccuracy"
  return(result_obj)
}

#' @title Accuracy method for ResamplePrediction
#'
#' @description Calculates Accuracy for a ResamplePrediction object.
#'
#' @param .prediction A ResamplePrediction object.
#' @export
EvaluatorAccuracy.ResamplePrediction <- function(.prediction) {
  res <- numeric(length(.prediction))
  for (split in names(.prediction)) {
    pre <- .prediction[[split]]$predictions
    assertNames(names(pre), subset.of = c("prediction", "truth"))
    res[split] <- mean(pre$prediction == pre$truth)
  }
  result <- mean(res)  # Assuming res is calculated as shown
  
  # Create a result object with a class
  result_obj <- list(value = result)
  class(result_obj) <- "EvaluatorAccuracy"
  return(result_obj)
}

#' @title 'print' method for `EvaluatorAccuracy`
#' 
#' @description This method prints the evaluator name and configuration
#' @param x An object of class `EvaluatorAccuracy`
#' @param ... Additional arguments
#' @export
print.EvaluatorAccuracy <- function(x, ...) {
  cat("Evaluator: Accuracy\n")
  cat("Configuration: ()\n")
  cat("Value:", x$value, "\n")
}

#' @title AUC generic function
#'
#' @description This function calculates the area under the ROC curve.
#'
#' @param .prediction An object containing the predictions and the truth.
#' @export
EvaluatorAUC <- function(.prediction) {
  UseMethod("EvaluatorAUC")
}

#' @title AUC method for data.frame
#'
#' @description Calculates AUC for a data.frame.
#'
#' @param .prediction A data.frame with prediction probabilities and actual binary outcomes.
#' @export
EvaluatorAUC.data.frame <- function(.prediction) {
  requireNamespace("pROC", quietly = TRUE)
  roc_response <- pROC::roc(response = .prediction$truth, predictor = .prediction$prediction)
  result <- pROC::auc(roc_response)
  
  # Create a result object with a class
  result_obj <- list(value = result)
  class(result_obj) <- "EvaluatorAUC"
  return(result_obj)
}

#' @title AUC method for ResamplePrediction
#'
#' @description Calculates AUC for a ResamplePrediction object.
#'
#' @param .prediction A ResamplePrediction object.
#' @export
EvaluatorAUC.ResamplePrediction <- function(.prediction) {
  requireNamespace("pROC", quietly = TRUE)
  res <- numeric(length(.prediction))
  for (split in names(.prediction)) {
    pre <- .prediction[[split]]$predictions
    assertNames(names(pre), subset.of = c("prediction", "truth"))
    roc_response <- pROC::roc(response = pre$truth, predictor = pre$prediction)
    res[split] <- pROC::auc(roc_response)
  }
  result <- mean(res)  # Assuming res is calculated as shown
  
  # Create a result object with a class
  result_obj <- list(value = result)
  class(result_obj) <- "EvaluatorAUC"
  return(result_obj)
}

#' @title 'print' method for `EvaluatorAUC`
#' 
#' @description This method prints the evaluator name and configuration
#' @param x An object of class `EvaluatorAUC`
#' @param ... Additional arguments
#' @export
print.EvaluatorAUC <- function(x, ...) {
  cat("Evaluator: Area Under the ROC Curve\n")
  cat("Configuration: ()\n")
  cat("Value:", x$value, "\n")
}

evl <- list2env(
  mae = EvaluatorMAE,
  rmse = EvaluatorRMSE,
  accuracy = EvaluatorAccuracy,
  auc = EvaluatorAUC
)

# Generic evaluate function
evaluate <- function(evaluator, .prediction, .dataset = NULL, .model = NULL, ...) {
  evaluator(.prediction, .dataset, .model, ...)
}
