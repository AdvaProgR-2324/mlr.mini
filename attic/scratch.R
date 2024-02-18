test <- function(a, b) a + b

test <- structure(
  test,
  class = c("Test", "Anna", class(test))
)

print.Test <- function(x, ...){
  args <- names(formals(x))
  print(args)
  print("Weee")
  invisible(x)
}
