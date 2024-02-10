#' @include InducerXgboost.R

#' @title Inducer S3 class
#' @export
InducerConstructer <- function(configuration, method) {
  # TO DO: Check validity of hyperparameters!
  assertList(configuration, names = "named")
  assertCharacter(method, len=1)
  hyperparameters <- names(configuration)
  Inducer <- structure(
    list(
      method = method,
      hyperparameters = hyperparameters,
      configuration = configuration
    ),
    class =  c(paste0("Inducer", method), "Inducer")
  )
  Inducer
}

#'  @title print Inducer.
#'  
#'  @description
#'  print information about inducer.
#'  @export
print.Inducer <- function(x,...) {
  config <- do.call(paste, c(list(x$hyperparameters, x$configuration),
                             list(sep=" = ", collapse=", ")))
  cat(sprintf("Inducer: %s\nConfiguration: %s", x$method, config))
  invisible(x)
}

# Hyperparameter generic (BIG TODO):
hyperparameters <- function(x) UseMethod("hyperparameters")
hyperparameters.Inducer <- function(x) {
  x$hyperparameters
}



#' @title Create copy of an inducer.
#' 
#' @description generic for `Inducer` class that produces copies of instances of
#' `Inducer` class that contain a new hyperparameter configuration.
#'  
#' @details This function creates a copy of the passed inducer with new config-
#'  uration values. New configuration values which have not been passed during
#'  initialization are added to the copy, while already present configuration
#'  values are overwritten in the copy.
#' 
#' @param .inducer S3 object of class `Inducer`.
#' @param new_configuration Named List, containing the hyperparameter
#'  configuration
#'
#' @returns A new instance of class `Inducer` with new configuration values. If
#'  no new configurations are passed, the original inducer is returned.
#'
#' @examples
#' InducerXgboost <- InducerConstruct(configuration = list(nrounds = 100),
#'                                    method = "Xgboost")
#' NewInducerXgboost <- copy(InducerXgboost,
#'                           new_configuration = list(nrounds = 20, max_depth = 6))
#' print(NewInducerXgboost)
#' NewInducerXgboost <- copy(InducerXgboost)
#' print(NewInducerXgboost)
#' @export
copy <- function(.inducer, new_configuration) UseMethod("copy")
copy.Inducer <- function(.inducer, new_configuration) {
  # TO DO: Check validity of hyperparameters!
  if (missing(new_configuration)) {
    return(.inducer)
  }
  assertList(new_configuration, unique = TRUE, names = "named")
  modified_config <- .inducer$configuration
  for (param_name in names(new_configuration)) {
    modified_config[[param_name]] <- new_configuration[[param_name]]
  }
  # Create copy of the original Inducer with new configuration and hyperparams:
  NewInstance <- InducerConstructer(configuration = modified_config,
                                    method = .inducer$method)
  return(NewInstance)
}



#' @title Accessor for the `configuration` of an inducer.
#' @param x S3 object of class inducer.
#' @returns Named list of hyperparameter configurations.
#' @export
configuration <- function(x) UseMethod("configuration")
configuration.Inducer <- function(x) x$configuration


#' @title Setter for the `configuration` of an inducer.
#' @description Set new configuration values for the hyperparameters of a given
#'  inducer.
#' @export
`configuration<-` <- function(object, value) {
  UseMethod("configuration<-", object)
}
`configuration<-` <- function(object, value) {
  assertList(value, unique = TRUE, names = "named")
  object <- copy(object, value)
  return(object)
}



#' @title Fit a model defined by an `Inducer` object on data defined by a `Dataset`
#'     object.
#' @param .inducer S3 object of class Inducer.
#' @param .data S3 object of class Dataset.
#' @returns S3 object of class `Model`.
#' @export
fit <- function(.inducer, .data,...) UseMethod("fit")
fit.Inducer <- function(.inducer, .data, ...) {
  assertClass(.inducer, classes = c("Inducer"))
  assertClass(.data, classes = c("Dataset"))
  
  task <- stringr::str_to_title(.data$type)
  training_data <- .data$data
  response <- .data$target
  y <- training_data[, (names(training_data) != response)]
  
  argdots <- list(...)
  if (any(!(.inducer$hyperparameters %in% names(argdots)))) {
    configuration(.inducer) <- argdots
  }
  
  model <- fit2(.inducer, task = task, training_data = training_data, response = response)
  
  structure(
    list(
      inducer = .inducer,
      data = .data,
      X = training_data,
      y = y,
      model = model,
      task = task
    ),
    class = "Model"
  )
}

print.Model <- function(x,...) {
  cat(sprintf('%s Model: "%s" fitted on "%s" dataset.', x$task, x$inducer$method,
          x$data$name))
}


# Test:
# meep <- fit(InducerXgboost, cars.data, eta = 0.2, nrounds = 10, verbose = 0)
# meep
# meep$inducer
# meep$data
# meep$model
# meep$model$feature_names
