

test <- function(...){
  self <- sys.function()
  args <- as.list(sys.call())[-1]
  formals(self)[names(args)] <- as.pairlist(args)
  class(self) <- c("test", class(self))
  self
}
test <- structure(test, class = c("test", class(test)))



print.test <- function(x,...) {
  print("Whee")
}

test
test(a=4)

formals(test)
formals(test(a=4))

