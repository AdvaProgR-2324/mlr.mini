
#' @title Hyperparameters S3 class
#' 
#' @description A set of functions to define hyperparameters for a model
#' 
#' @param ... A set of hyperparameters
#' @examples 
#' hpx <- hp(x = p_num(0, 1), y = p_int(1, Inf), z = p_fct(letters))
#' hpx
#' @export
hp <- function(...) {
  name <- names(list(...))
  type <- sapply(list(...), function(x) x[[1]])
  range <- sapply(list(...), function(x) x[[2]])
  
  
  structure(list(
    name = name,
    type = type,
    range = range
  ), class = "hp")
}

#' @title 'print' methode for `hp` class
#' 
#' @description Print an hp object
#' 
#' @param x An hp object
#' @param ... Additional arguments
#' @examples
#' hpx <- hp(x = p_num(0, 1), y = p_int(1, Inf), z = p_fct(letters))
#' print(hpx)
#' @export
print.hp <- function(x, ...) {
  print(data.frame(name = x$name, type = unlist(x$type), range = I(x$range), stringsAsFactors = FALSE))
  invisible(x)
}

#' @title Hyperparameter check
#' 
#' @description Check if an element is in the hyperparameter space
#' 
#' @param check A list of hyperparameters
#' @param hp A hyperparameter space
#' @examples
#' hpx <- hp(x = p_num(0, 1), y = p_int(1, Inf), z = p_fct(letters))
#' hpCheck(list(x = 1, y = 1, z = "a"), hpx)
#' @export

hpCheck <- function(check, hp) {
  assert_list(check)
  assert_class(hp, "hp")

  ret <- TRUE

  for (i in names(check)) {
    type <- hp$type[i == hp$name]
    temp <- switch(type,
           num = check_numeric(check[[i]], lower = hp$range[[i]][1], upper = hp$range[[i]][2]),
           int = check_integerish(check[[i]], lower = hp$range[[i]][1], upper = hp$range[[i]][2]),
           fct = check_choice(check[[i]], choices = unlist(hp$range[[i]])))
    if (temp != TRUE) {
      return(temp) # to return all errors replace with print()
      ret <- FALSE
    }
  }
  if (ret) return(ret)
}

#' @title Numeric Range
#' 
#' @description Define ranges for numeric hyperparameters
#' 
#' @param lower Lower bound
#' @param upper Upper bound
#' @examples
#' p_num(0, 1)
#' @export
p_num <- function(lower, upper) {
  assert_numeric(lower)
  assert_numeric(upper)
  
  list(type = "num", range = c(lower, upper))
}

#' @title Integer Range
#' 
#' @description Define ranges for integer hyperparameters
#' 
#' @param lower Lower bound
#' @param upper Upper bound
#' @examples
#' p_int(1, Inf)
#' @export
p_int <- function(lower, upper) {
  assert_true(upper > lower)
  assert_true(isTRUE(check_integerish(lower)) | lower %in% c(-Inf, Inf))
  assert_true(isTRUE(check_integerish(upper)) | upper %in% c(-Inf, Inf))
  
  list(type = "int", range = c(lower, upper))
}

#' @title Factor Range
#' 
#' @description Define ranges for factor hyperparameters
#' 
#' @param ... A set of factors
#' @examples
#' p_fct(letters)
#' @export
p_fct <- function(...) {
  assert_atomic(...)
  
  list(type = "fct", range = list(...))
}
