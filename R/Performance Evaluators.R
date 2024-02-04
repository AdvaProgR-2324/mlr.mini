#Performance Evaluators
EvaluatorMAE <- function(.prediction, .dataset = NULL, .model = NULL, ...) {
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

# Example usage
prediction <- c(1, 2, 3)  # Example prediction values
actual <- c(1, 2, 4)      # Example actual values

# Create a dataset (modify this according to your actual data structure)
dataset <- data.frame(actual = actual)

# Using the evaluator
mae_result <- evl$mae(prediction, dataset)
print(mae_result)

identical(evl$mae, EvaluatorMAE)