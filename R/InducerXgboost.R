#' @include constructFormula.R

#' @title Fit an XGBoost model.
#' 
#' @description This internal function uses an inducer of class `InducerXgboost`
#'  to fit an XGboost model to the given training dataset.
#'
#' @param .inducer An S3 object of class `InducerXGBoost`.
#' @param training_data Data.Frame or Matrix extracted from the `data` field
#'  of the `Dataset` class.
#' @param response String that stores the name of the target variable and is
#'  extracted from the `target` field of the `Dataset` class.
#' @param task String that is either of value `"regression"` or `"classification"`
#'
#' @details We use `xgboost::xgboost()` for the model fitting.
#' @export
fit2 <- function(.inducer, task, training_data, response) UseMethod("fit2")

#' @title Fit an XGBoost model.
#' @param .inducer An S3 object of class `InducerXGBoost`.
#' @param task String that is either of value `"regression"` or `"classification"`
#' @param training_data Data.Frame extracted from the `data` field of the `Dataset` class.
#' @param response String that stores the name of the target variable
#' @export
fit2.InducerXGBoost <- function(.inducer, task, training_data, response) {
  
  # Define objective based on the task:
  if (task == "Regression") {
    objective <- "reg:squarederror"
  } else if (task == "Classification") {
    training_data[[response]] <- as.factor(training_data[[response]])
    if (nlevels(training_data[[response]]) == 2) {
        objective <-"binary:logistic"
        task <- "Binary Classification"
    } else {
        objective <-"multi:softprob"
        task <- "Multiclass Classification"
    }
  }
  
  # Preparation of the data (XGBoost manages only numeric vectors!):
  form <- constructFormula(response,
                           names(training_data)[(names(training_data) != response)])
  sparse_matrix <- Matrix::sparse.model.matrix(update(form, . ~ . - 1), data = training_data)
  
  # Train the model:
  do.call(xgboost::xgboost,
          c(list(data = sparse_matrix, label = training_data[[response]], objective = objective),
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
