# Constructer for Dataset()
Dataset <- function(data, target, name = deparse(substitute(data))) {
  # Input checks
  assertDataFrame(data)
  assertChoice(target, colnames(data))
  
  # Infer the task
  if (is.numeric(data[[target]])) {
    type <- "Regression"
  } else {
    type <- "Classification"
  }
  
  structure(list(
    data = data,
    target = target,
    type = type,
    name = name
  ), class = c("Dataset", paste0("Dataset", type)))
}

# Define the '[' method for subsetting
`[.Dataset` <- function(x, i, j) {
  # Input checks
  assertClass(x, "Dataset")
  # check if i and j are valid
  # to do
  
  # Check if 'j' is provided, if not select all columns
  if (missing(j)) {
    j <- colnames(x$data)
  }
  
  # Ensure that the target column is not removed
  if (!(x$target %in% j)) {
    stop(sprintf("Cannot remove target column %s", x$target))
  }
  
  new_data <- x$data[i, j, drop = FALSE]
  new_target <- x$target
  
  return(Dataset(data = new_data, target = new_target, name = x$name))
}


# Define the 'as.data.frame' method
as.data.frame.Dataset <- function(x, columns = "all") {
  # Input checks
  assertClass(x, "Dataset")
  assertChoice(columns, c("all", "target", "features"))
  
  switch(columns,
         "all" = x$data,
         "target" = x$data[, x$target, drop = FALSE],
         "features" = x$data[, setdiff(names(x$data), x$target), drop = FALSE])
}

# Define the 'metainfo' generic function
metainfo <- function(x) {
  info <- list(
    name = x$name,
    features = setdiff(names(x$data), x$target),
    targets = setNames(class(x$data[[x$target]]), x$target),
    nrow = nrow(x$data),
    type = x$type,
    missings = any(is.na(x$data))
  )
  class(info) <- "DatasetInfo"
  return(info)
}

# Define the 'print' method
print.Dataset <- function(x, ...) {
  cat("Dataset \"", x$name, "\", predicting \"", x$target, "\" (", x$type, ")\n", sep = "")
  print(x$data)
}

# Example usage
cars.data <- Dataset(data = cars, target = "dist")
print(cars.data)
class(cars.data)

cars.data[c(1, 2, 3, 4), ]
cars.data[c(1, 2), "dist"]
cars.data[, "speed"]

metainfo(cars.data)
