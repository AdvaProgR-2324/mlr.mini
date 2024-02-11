constructFormula <- function(outcome, covariates, env = parent.frame()) {
  # your code
  assertString(outcome)
  assertCharacter(covariates, min.len = 1, any.missing = FALSE)
  assertEnvironment(env)
  
  covariates <- lapply(covariates, as.name)
  # rhs <- covariates[[1]]
  # for (cov in covariates[-1]) {
  #   rhs <- call("+", rhs, as.name(cov))
  # }
  rhs <- Reduce(function(x, y) call("+", x, y), covariates)
  result <- call("~", as.name(outcome), rhs)
  # class(result) <- "formula"
  # environment(result) <- env
  # result
  structure(
    result,
    class = "formula",
    .Environment = env
  )
}

