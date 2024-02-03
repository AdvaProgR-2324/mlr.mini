#' @include Inducer.R


fit2 <- function(.inducer, .data,...) UseMethod("fit2")
#' @title Fit an XGBoost model.
#' 
#' @description This internal function uses an inducer of class `InducerXgboost`
#' to fit an XGboost model to the given training dataset.
#'
#' @param .inducer An S3 object of class `InducerXGboost`.
#' @param training_data Data.Frame or Matrix extracted from the `data` field
#'  of the `Dataset` class.
#' @param response String that stores the name of the target variable and is
#'  extracted from the `target` field of the `Dataset` class.
#' @param task String that is either of value `"regression"` or `"classification"`
#'
#' @details
#'  We use `xgboost::xgboost()` for the model fitting.

fit2.InducerXgboost <- function(.inducer, task, training_data, response) {
  # Define objective based on the type of the data:
  objective <- if (task == "Regression") {
      "reg:squarederror"
    # } else if (task == "multiclass") {
    #   "multi:softprob"
    } else if (task == "Classification") {
      "binary:logistic"
    }
  # Preparation of the data (XGBoost manages only numeric vectors!):
  y <- training_data[, response]
  sparse_matrix <- Matrix::sparse.model.matrix(y ~ ., data = training_data)[, -1]
  
  # Train the model:
  do.call(xgboost::xgboost,
          c(list(data = sparse_matrix, label = y, objective = objective),
            configuration(.inducer)))
}


# Later:
# InducerXgboost <- function(.data, ...) {
#   if (missing(.data)) {
#     self <- sys.function()
#     args <- as.list(sys.call())[-1]
#     formals(self)[names(args)] <- as.pairlist(args)
#     self
#   } else {
#     fit(InducerXgboost, ...)
#   }
# }
