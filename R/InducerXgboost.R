#' @include Inducer.R

source(here::here("R/Inducer.R"))  # TO DO: delete this

InducerXgboost <- InducerConstructer(hyperparam = c("nrounds", "max_depth", "eta"),
                                     config = c(100, 6, 0.3),
                                     method = "Xgboost")
class(InducerXgboost)
InducerXgboost
hyperparameters(InducerXgboost)
configuration(InducerXgboost)
configuration(InducerXgboost) <- list(nrounds = 20, test = TRUE)
hyperparameters(InducerXgboost)
InducerXgboost

fit2.InducerXgboost <- function(.inducer, .data,...) {
  args <- list(...)
  if (any(!(inducer$hyperparameters %in% names(args)))) {
    print("Add additional hyperparameter")
  }
  # To Do: Add new configuration
  
  
  
  # Returns an S3 object with the following fields:
  # - model parameters
  # - y: target variable used for fitting
  # - X: data the model was trained on (w/o y)
  # - inducer from input
  # - configuration
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
