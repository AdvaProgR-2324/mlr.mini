#' @include constructFormula.R

#' @title Fit an XGBoost model.
#' 
#' @description This internal function uses an inducer of class `InducerXGBoost`
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
  do.call(
    xgboost::xgboost,
    c(list(data = sparse_matrix, label = training_data[[response]], objective = objective),
      configuration(.inducer))
  )
}


#' @title Construct or fit an XGBoost Inducer.
#' 
#' @param .data Object of class `Dataset`. Optional.
#' @param ... Additional parameters specifying the hyperparameter configuration
#' of the XGBoost model. If none are given, the default configuration of
#' `xgboost::xgboost` is used for the model fitting process.
#' 
#' @returns A S3 object of either class `Model` (`.data` was given) or of class
#' `InducerXGBoost` (`.data` was not given).
#' 
#' @details
#' If `.data` argument is passed, an XGBoost model will be trained on the given
#' dataset using the passed hyperparameter configuration. Otherwise, an inducer of
#' class `InducerXGBoost` is created with the given hyperparameter configuration.
#' 
#' @examples
#' InducerXGBoost()
#' InducerXGBoost(verbose = 0)
#' cars.data <- Dataset(data = cars, target = "dist")
#' InducerXGBoost(cars.data, verbose = 0, nrounds = 10)
#' 
#' @export 
InducerXGBoost <- function(.data, ...) {
  self <- sys.function()
  args <- as.list(sys.call())
  if (missing(.data)) {
    kwargs <- args[-1]
    formals(self)[names(kwargs)] <- as.pairlist(kwargs)
    inducer <- InducerConstructer(configuration = kwargs,
                                  method = "XGBoost")
    return(inducer)
  } else {
    kwargs <- args[-c(1, 2)]
    inducer <- InducerConstructer(configuration = kwargs,
                                  method = "XGBoost")
    return(fit(inducer, .data = .data, ...))
  }
}
