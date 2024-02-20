#' @include constructFormula.R

#' @title Fit a GLM.
#' 
#' @description This internal function uses an inducer of class `InducerGlm`
#'  to fit a Random Forest model to the given training dataset.
#'
#' @param .inducer An S3 object of class `InducerGlm`.
#' @param training_data Data.Frame or Matrix extracted from the `data` field
#'  of the `Dataset` class.
#' @param response String that stores the name of the target variable and is
#'  extracted from the `target` field of the `Dataset` class.
#' @param task String that is either of value "Regression" or "Classification".
#'
#' @export
fit2.InducerGlm <- function(.inducer, task, training_data, response) {
  form <- constructFormula(response,
                           names(training_data)[(names(training_data) != response)])
  if (task == "Regression") {
    link <- "gaussian"
  } else if (task == "Classification") {
    link <- "binomial"
  }
  do.call(
    glm,
    c(list(formula = form, data = training_data, family = link),
      configuration(.inducer))
  )
}


#' @title Construct or fit a GLM Inducer.
#' 
#' @param .data Object of class `Dataset`. Optional.
#' @param ... Additional parameters specifying the hyperparameter configuration
#' of the GLM model. If none are given, the default configuration of
#' `stats::glm` is used for the model fitting process.
#' 
#' @returns A S3 object of either class `Model` (`.data` was given) or of class
#' `InducerGlm` (`.data` was not given).
#' 
#' @details
#' If `.data` argument is passed, an Random Forest model will be trained on the given
#' dataset using the passed hyperparameter configuration. Otherwise, an inducer of
#' class `InducerGlm` is created with the given hyperparameter configuration.
#' 
#' @export 
InducerGlm <- function(.data, ...) {
  self <- sys.function()
  args <- as.list(sys.call())
  if (missing(.data)) {
    kwargs <- args[-1]
    formals(self)[names(kwargs)] <- as.pairlist(kwargs)
    inducer <- InducerConstructer(configuration = kwargs,
                                  method = "Glm")
    return(inducer)
  } else {
    kwargs <- args[-c(1, 2)]
    inducer <- InducerConstructer(configuration = kwargs,
                                  method = "Glm")
    return(fit(inducer, .data = .data, ...))
  }
}
