#' @include Inducer.R

source(here::here("R/Inducer.R"))  # TO DO: delete this

InducerXgboost <- InducerConstructer(configuration = list(nrounds = 100, 
                                                          max_depth = 6, 
                                                          eta = 0.3),
                                     method = "Xgboost")

fit2 <- function(.inducer, .data,...) UseMethod("fit2")
fit2.InducerXgboost <- function(.inducer, .data) {
  
  objective <- if (.data$type == "regression") {
      "reg:squarederror"
    } else if (.data$type == "multiclass") {
      "multi:softprob"  # maybe turn this into "multi:softmax" ?
    } else if (.data$type == "classification") {
      "binary:logistic"
    }
  
   

  
  
  
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
