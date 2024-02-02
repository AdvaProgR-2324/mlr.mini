# Constructer for Inducer Class:
InducerConstructer <- function(hyperparameters, configuration, method) {
  configuration_list <- as.list(configuration)
  names(configuration_list) <- hyperparameters
  Inducer <- structure(
    list(
      method = method,
      hyperparameters = hyperparameters,
      configuration = configuration_list
    ),
    class =  c(paste0("Inducer", method), "Inducer")
  )
  Inducer
}

# Printer for Inducer Class:
print.Inducer <- function(x,...) {
  config <- do.call(paste, c(list(x$hyperparameters, x$configuration), list(sep=" = ", collapse=", ")))
  cat(sprintf("Inducer: %s\nConfiguration: %s", x$method, config))
  invisible(x)
}

# Hyperparameter generic:
hyperparameters <- function(x) UseMethod("hyperparameters")
hyperparameters.Inducer <- function(x) {
  x$hyperparameters
}


# Copy generic (helper function):
copy <- function(original, new_hyperparameters) UseMethod("copy")
copy.Inducer <- function(original, new_hyperparameters) {
  # This helper method creates a "copy" (i.e. new instance of inducer class) of
  # the original Inducer with the new hyperparameter configuration:
  if (missing(new_hyperparameters)) {
    return(original)
  }
  assertList(new_hyperparameters, unique = TRUE)
  # TODO: Assertion on hyperparameters?
  modified_config <- original$configuration
  modified_hyperparam <- original$hyperparameter
  for (param_name in names(new_hyperparameters)) {
    modified_config[[param_name]] <- new_hyperparameters[[param_name]]
    if (!(param_name %in% modified_hyperparam)) {
      modified_hyperparam <- c(modified_hyperparam, param_name)
    }
  }
  # Create copy of the original Inducer with new configuration and hyperparams:
  NewInstance <- InducerConstructer(configuration = modified_config,
                                    hyperparameters = modified_hyperparam,
                                    method = original$method)
  return(NewInstance)
}


# Configuration generics:
configuration <- function(x) UseMethod("configuration")
configuration.Inducer <- function(x) x$configuration

`configuration<-` <- function(object, value) {
  UseMethod("configuration<-", object)
}
`configuration<-` <- function(object, value) {
  assertList(value, unique = TRUE)
  if (anyNA(names(value))) {
    stop("List of configuration values must have names for every hyperparameter!")
  }
  object <- copy(object, value)
  return(object)
}

