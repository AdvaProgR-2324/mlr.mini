# Constructer for Inducer Class:
InducerConstructer <- function(hyperparam, config,
                               method = c("Xgboost", "RandomForest", "Dummy")) {
  Inducer <- structure(
    list(
      method = method,
      hyperparam = hyperparam,
      config = config
    ),
    class =  c("Inducer", paste0("Inducer", method))
  )
  Inducer
  # Maybe access `ind` environment directly from this constructer?
}

# Printer:
print.Inducer <- function(x,...) {
  config <- do.call(paste, c(list(x$hyperparam, x$config), list(sep=" = ", collapse=", ")))
  cat(sprintf("Inducer: %s\nConfiguration: %s", x$method, config))
  invisible(x)
}

# Hyperparameter generic:
hyperparameters <- function(x) {
  UseMethod("hyperparameters")
}
hyperparameters.Inducer <- function(x) {
  x$hyperparam
}


InducerXgboost <- InducerConstructer(hyperparam = c("nrounds", "max_depth", "eta"),
                                     config = c(100, 6, 0.3),
                                     method = "Xgboost")
class(InducerXgboost)
InducerXgboost
hyperparameters(InducerXgboost)


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
