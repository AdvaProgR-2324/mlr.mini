#' @title Dataset S3 class
#' 
#' @description A class for storing datasets
#' 
#' @param data A data frame containing the data
#' @param target The name of the target column
#' @param name The name of the dataset, by default the name of the given data object
#' @example
#' cars.data <- Dataset(data = cars, target = "dist")
#' class(cars.data)
#' @export
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
  
  # Target should be the first column
  data_ordered <- data[, c(target, setdiff(names(data), target))]
  
  structure(list(
    data = data_ordered,
    target = target,
    type = type,
    name = name
  ), class = c("Dataset", paste0("Dataset", type)))
}

#' @title 'print' method for Dataset objects
#' 
#' @description Print a Dataset object
#' 
#' @param x A Dataset object
#' @example
#' cars.data <- Dataset(data = cars, target = "dist")
#' print(cars.data)
#' @export
print.Dataset <- function(x) {
  cat("Dataset \"", x$name, "\", predicting \"", x$target, "\" (", x$type, ")\n", sep = "")
  print(x$data)
}

#' @title '[' method for subsetting Dataset objects
#' 
#' @description Subset a Dataset object
#' 
#' @param x A Dataset object
#' @param i A vector of row indices
#' @param j A vector of column indices or column names
#' @example
#' cars.data <- Dataset(data = cars, target = "dist")
#' cars.data[c(1, 2, 3, 4), ]
#' cars.data[c(1, 2), "dist"]
#' @export
`[.Dataset` <- function(x, i, j) {
  # Input checks
  assertClass(x, "Dataset")
  if(!missing(i)) {
    assertNumeric(i, lower = 1, upper = nrow(x$data))
  }
  
  # Check if 'j' is provided, if not select all columns
  if (missing(j)) {
    j <- colnames(x$data)
  }
  # check if j is valid (valid colnames or indices)
  if (is.character(j)) {
    assertTRUE(all(j %in% colnames(x$data)))
  } else {
    assertNumeric(j, lower = -ncol(x$data), upper = ncol(x$data))
  }
  
  new_data <- x$data[i, j, drop = FALSE]
  # Ensure that the target column is not removed
  if (!(x$target) %in% colnames(new_data)) {
    stop(sprintf("Cannot remove target column %s", x$target))
  }
  
  return(Dataset(data = new_data, target = x$target, name = x$name))
}

#' @title 'as.data.frame' method for Dataset objects
#' 
#' @description Convert a Dataset object to a data frame
#' 
#' @param x A Dataset object
#' @param columns Which columns to include in the data frame: "all", "target", "features". By default "all".
#' 
#' @example
#' cars.data <- Dataset(data = cars, target = "dist")
#' head(as.data.frame(cars.data))
#' @export
as.data.frame.Dataset <- function(x, columns = "all") {
  # Input checks
  assertClass(x, "Dataset")
  assertChoice(columns, c("all", "target", "features"))
  
  switch(columns,
         "all" = x$data,
         "target" = x$data[, x$target, drop = FALSE],
         "features" = x$data[, setdiff(names(x$data), x$target), drop = FALSE])
}

# Maybe move the following to the metainfo file
# Define the 'metainfo' generic function
metainfo <- function(x) {UseMethod("metainfo")}
#' @title 'metainfo' method for Dataset objects
#' 
#' @description Get the metainfo of a Dataset object (name, features, targets, nrow, type, missings)
#' 
#' @param x A Dataset object
#' @example
#' cars.data <- Dataset(data = cars, target = "dist")
#' metainfo(cars.data)
#' @export
metainfo.Dataset <- function(x) {
  feature_names <- setdiff(names(x$data), x$target)
  features <- class(x$data[, feature_names])
  names(features) <- feature_names
  
  info <- list(
    name = x$name,
    features = features,
    targets = setNames(class(x$data[[x$target]]), x$target),
    nrow = nrow(x$data),
    type = x$type,
    missings = any(is.na(x$data))
  )
  class(info) <- "DatasetInfo"
  return(info)
}

# Example usage
cars.data <- Dataset(data = cars, target = "dist")
print(cars.data)
class(cars.data)

cars.data[c(1, 2, 3, 4), ]
cars.data[c(1, 2), "dist"]
cars.data[, "speed"]

metainfo(cars.data)
