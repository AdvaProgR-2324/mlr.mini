#Performance Evaluators
EvaluatorMAE <- function(.prediction, .dataset = NULL, .model = NULL, ...) {
  if (is.null(.dataset)) {
    stop("Dataset must be provided for MAE evaluation.")
  }
  # Assuming .prediction is a vector of predictions and 
  # the actual values are in .dataset$actual 
  mean(abs(.prediction - .dataset$actual))
}

# Set the class for pretty printing
class(EvaluatorMAE) <- c("EvaluatorMAE", class(EvaluatorMAE))
print.EvaluatorMAE <- function(x, ...) {
  cat("Evaluator: Mean Absolute Error\n")
  cat("Configuration: ()\n")
}
evl <- new.env()

evl$mae <- EvaluatorMAE

EvaluatorRMSE <- function(.prediction, .dataset = NULL, .model = NULL, ...) {
  if (is.null(.dataset)) {
    stop("Dataset must be provided for RMSE evaluation.")
  }
  sqrt(mean((.prediction - .dataset$actual)^2))
}

class(EvaluatorRMSE) <- c("EvaluatorRMSE", class(EvaluatorRMSE))
print.EvaluatorRMSE <- function(x, ...) {
  cat("Evaluator: Root Mean Squared Error\n")
  cat("Configuration: ()\n")
}

evl$rmse <- EvaluatorRMSE

EvaluatorAccuracy <- function(.prediction, .dataset = NULL, .model = NULL, ...) {
  if (is.null(.dataset)) {
    stop("Dataset must be provided for Accuracy evaluation.")
  }
  mean(.prediction == .dataset$actual)
}

class(EvaluatorAccuracy) <- c("EvaluatorAccuracy", class(EvaluatorAccuracy))
print.EvaluatorAccuracy <- function(x, ...) {
  cat("Evaluator: Accuracy\n")
  cat("Configuration: ()\n")
}

evl$accuracy <- EvaluatorAccuracy

EvaluatorAUC <- function(.prediction, .dataset = NULL, .model = NULL, ...) {
  if (is.null(.dataset)) {
    stop("Dataset must be provided for AUC evaluation.")
  }
  
  requireNamespace("pROC", quietly = TRUE)
  roc_response <- pROC::roc(.dataset$actual, .prediction)
  pROC::auc(roc_response)
}

class(EvaluatorAUC) <- c("EvaluatorAUC", class(EvaluatorAUC))
print.EvaluatorAUC <- function(x, ...) {
  cat("Evaluator: Area Under the ROC Curve\n")
  cat("Configuration: ()\n")
}

evl$auc <- EvaluatorAUC


# Generic evaluate function
evaluate <- function(evaluator, .prediction, .dataset = NULL, .model = NULL, ...) {
  evaluator(.prediction, .dataset, .model, ...)
}

