#' @title Fit a Random Forest model.
#' 
#' @description This internal function uses an inducer of class `InducerRandomForest`
#'  to fit a Random Forest model to the given training dataset.
#'
#' @param .inducer An S3 object of class `InducerRandomForest`.
#' @param training_data Data.Frame or Matrix extracted from the `data` field
#'  of the `Dataset` class.
#' @param response String that stores the name of the target variable and is
#'  extracted from the `target` field of the `Dataset` class.
#' @param task String that is either of value `"regression"` or `"classification"`
#'
#' @details We use `randomForest::randomForest()` for the model fitting.
#' @export
fit2.InducerRandomForest <- function(.inducer, task, training_data, response) {
  response_vector <- training_data[,response]
  do.call(
    randomForest::randomForest,
    c(list(formula = response ~ ., data = training_data, y = response_vector),
      configuration(.inducer))
  )
}


#' @title Construct or fit a Random Forest Inducer.
#' 
#' @param .data Object of class `Dataset`. Optional.
#' @param ... Additional parameters specifying the hyperparameter configuration
#' of the Random Forest model. If none are given, the default configuration of
#' `randomForest::randomForest` is used for the model fitting process.
#' 
#' @returns A S3 object of either class `Model` (`.data` was given) or of class
#' `InducerRandomForest` (`.data` was not given).
#' 
#' @details
#' If `.data` argument is passed, an Random Forest model will be trained on the given
#' dataset using the passed hyperparameter configuration. Otherwise, an inducer of
#' class `InducerRandomForest` is created with the given hyperparameter configuration.
#' 
#' @export 
InducerRandomForest <- function(.data, ...) {
  self <- sys.function()
  args <- as.list(sys.call())[-1]
  formals(self)[names(args)] <- as.pairlist(args)
  inducer <- InducerConstructer(configuration = args,
                                method = "RandomForest")
  if (missing(.data)) {
    return(inducer)
  } else {
    fit(inducer, .data = .data, ...)
  }
}