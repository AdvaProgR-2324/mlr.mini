#Performance Evaluators
EvaluatorMAE <- function(.prediction, .dataset = NULL, .model = NULL, ...) {
  if (is.null(.dataset)) {
    stop("Dataset must be provided for MAE evaluation.")
  }
  # Assuming .prediction is a vector of predictions and 
  # the actual values are in .dataset$actual (modify as needed)
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

# Generic evaluate function
evaluate <- function(evaluator, .prediction, .dataset = NULL, .model = NULL, ...) {
  evaluator(.prediction, .dataset, .model, ...)
}

