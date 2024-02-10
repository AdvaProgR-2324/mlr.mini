# Constructer for Inducer Class:
InducerConstructer <- function(configuration, method) {
  # TO DO: Check validity of hyperparameters!
  assertList(configuration, names = "named")
  assertCharacter(method, len=1)
  hyperparameters <- names(configuration)
  Inducer <- structure(
    function(configuration) {
      configuration
    },
    class =  c(paste0("Inducer", method), "Inducer")
  )
  Inducer
}


print.Inducer <- function(x,...) {
  config <- do.call(paste, c(list(x$hyperparameters, x$configuration),
                             list(sep=" = ", collapse=", ")))
  cat(sprintf("Inducer: %s\nConfiguration: %s", x$method, config))
  invisible(x)
}