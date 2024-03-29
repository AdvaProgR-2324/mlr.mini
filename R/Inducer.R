
#' @title Inducer S3 class.
#' @param configuration Named list of hyperparameters.
#' @param method String that stores the name of the method.
#' @export
InducerConstructer <- function(configuration, method) {
  # TO DO: Check validity of hyperparameters!
  assertList(configuration, names = "named", null.ok = TRUE)
  assertCharacter(method, len=1)
  hyperparameters <- names(configuration)
  Inducer <- structure(
    list(
      method = method,
      hyperparameters = hyperparameters,
      configuration = configuration
    ),
    class =  c(paste0("Inducer", method), "Inducer", "function")
  )
  Inducer
}

#' @title print method for `Inducer` class.
#' @description print information about inducer.
#' @param x S3 object of class `Inducer`.
#' @param ... Additional arguments.
#' @export
print.Inducer <- function(x, ...) {
  if (is.null(x$hyperparameters)) {
    config <- "default"
  }
  else{
    config <- do.call(paste, c(list(x$hyperparameters, x$configuration),
                               list(sep=" = ", collapse=", ")))
  }
  cat(sprintf("Inducer: %s\nConfiguration: %s", x$method, config))
  invisible(x)
}

# # Hyperparameter generic (BIG TODO):
# hyperparameters <- function(x) UseMethod("hyperparameters")
# hyperparameters.Inducer <- function(x) {
#   x$hyperparameters
# }



#' @title S3 generic to copy an inducer.
#' @description
#' Method that produces copies of instances of `Inducer` class that contain a
#' new hyperparameter configuration.
#' @param .inducer S3 object of class `Inducer`.
#' @param new_configuration Named List containing the hyperparameter
#' 
#' @export
copy <- function(.inducer, new_configuration) UseMethod("copy")

#' @title Create copy of an inducer.
#'   
#' @description
#'  This function creates a copy of the passed inducer with new configuration values.
#'  New configuration values which have not been passed during
#'  initialization are added to the copy, while already present configuration
#'  values are overwritten in the copy.
#' 
#' @param .inducer S3 object of class `Inducer`.
#' @param new_configuration Named List, containing the hyperparameter
#'  configuration.
#'
#' @returns A new instance of class `Inducer` with new configuration values. If
#'  no new configurations are passed, the original inducer is returned.
#'
#' @examples
#' InducerXgboost <- InducerConstructer(configuration = list(nrounds = 100),
#'                                    method = "Xgboost")
#' NewInducerXgboost <- copy(InducerXgboost,
#'                           new_configuration = list(nrounds = 20, max_depth = 6))
#' print(NewInducerXgboost)
#' NewInducerXgboost <- copy(InducerXgboost)
#' print(NewInducerXgboost)
#' @export
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
#' @description
#' S3 generic.
#' 
#' @param x S3 object.
#' @returns Named list of hyperparameter configurations.
#' @export
configuration <- function(x) UseMethod("configuration")

#' @title Access `configuration` of an inducer.
#' @param x S3 object of class `Inducer`.
#' @export 
configuration.Inducer <- function(x) x$configuration


#' @title Setter for the `configuration` of an inducer.
#' @description S3 generic. Set new configuration values for the hyperparameters of a given inducer.
#' @param object S3 object.
#' @param value Named list of configurations.
#' @export
`configuration<-` <- function(object, value) {
  UseMethod("configuration<-", object)
}
#' @title Set `configuration` of an inducer.
#' @param object S3 object of class `Inducer`.
#' @param value Named list of configurations.
#' @export
`configuration<-` <- function(object, value) {
  assertList(value, unique = TRUE, names = "named")
  object <- copy(object, value)
  return(object)
}


#' @title S3 (helper) generic for fitting specific Inducers.
#' 
#' @description This internal function uses a specific inducer of class `InducerXxx`
#'  to fit a corresponding model to the given training dataset.
#'
#' @param .inducer An S3 object of (sub-)class `InducerXxx`.
#' @param training_data Data.Frame or Matrix extracted from the `data` field
#'  of the `Dataset` class.
#' @param response String that stores the name of the target variable and is
#'  extracted from the `target` field of the `Dataset` class.
#' @param task String that is either of value `"regression"` or `"classification"`
#'
#' @export
fit2 <- function(.inducer, task, training_data, response) UseMethod("fit2")



#' @title S3 generic for model fit on inducers.
#' 
#' @param .inducer S3 object of class `Inducer`.
#' @param .data S3 object of class Dataset.
#' @param ... Additional arguments.
#' @returns S3 object of class `Model`.
#' @export
fit <- function(.inducer, .data,...) UseMethod("fit")

#' @title Fit a model defined by an `Inducer` object on data defined by a `Dataset` object.
#' @param .inducer S3 object of class `Inducer`.
#' @param .data S3 object of class `Dataset`.
#' @param ... Additional arguments.
#' @export
fit.Inducer <- function(.inducer, .data, ...) {
  assertClass(.inducer, classes = c("Inducer"))
  assertClass(.data, classes = c("Dataset"))
  
  training_time_start <- Sys.time()
  
  task <- stringr::str_to_title(.data$type)
  training_data <- .data$data
  response <- .data$target
  y <- training_data[, (names(training_data) != response)]
  
  argdots <- list(...)
  if (any(!(.inducer$hyperparameters %in% names(argdots)))) {
    configuration(.inducer) <- argdots
  }
  
  model <- fit2(.inducer, task = task, training_data = training_data, response = response)
  
  training_time_end <- Sys.time()
  
  structure(
    list(
      inducer = .inducer,
      data = .data,
      X = training_data,
      y = y,
      model = model,
      task = task,
      training_time_sec = as.numeric(difftime(training_time_end, training_time_start))
    ),
    class = c(paste0("Model", .inducer$method), paste0("Model", task), "Model")
  )
}

#' @title print method for `Model` class
#' @param x S3 object of class `Model`.
#' @param ... Additional arguments.
#' @export
print.Model <- function(x, ...) {
  cat(sprintf('%s Model: "%s" fitted on "%s" dataset.', x$task, x$inducer$method,
              x$data$name))
}