#' @title Dataset S3 class
#' 
#' @description A class for storing datasets
#' 
#' @param data A data frame containing the data
#' @param target The name of the target column
#' @param name The name of the dataset, by default the name of the given data object
#' @param type The task of the dataset: "classification" or "regression". If no task is provided, numerical target columns are considered as regression tasks, and non-numeric target columns as classification tasks.
#' @examples
#' cars.data <- Dataset(data = cars, target = "dist")
#' class(cars.data)
#' @export
Dataset <- function(data, target, name = deparse(substitute(data)), type) {
  # Input checks
  assertDataFrame(data)
  assertChoice(target, colnames(data))
  assertCharacter(name)
  if(!missing(type)) {
    assertChoice(type, c("classification", "regression"))
  }
  
  # Infer the task
  if (missing(type)) {
    type <- if (is.numeric(data[[target]])) {
      "regression"
    } else {
      "classification"
    }
  }
  
  # Target should be the first column
  data_ordered <- data[, c(target, setdiff(names(data), target))]
  
  structure(list(
    data = data_ordered,
    target = target,
    type = type,
    name = name
  ), class = c(paste0("Dataset", tools::toTitleCase(type)), "Dataset"))
}

#' @title 'print' method for Dataset objects
#' 
#' @description Print a Dataset object
#' 
#' @param x A Dataset object
#' @param ... Additional arguments
#' @examples
#' cars.data <- Dataset(data = cars, target = "dist")
#' print(cars.data)
#' @export
print.Dataset <- function(x, ...) {
  cat("Dataset \"", x$name, "\", predicting \"", x$target, "\" (", tools::toTitleCase(x$type), ")\n", sep = "")
  print(x$data, ...)
}

#' @title '[' method for subsetting Dataset objects
#' 
#' @description Subset a Dataset object
#' 
#' @param x A Dataset object
#' @param i A vector of row indices
#' @param j A vector of column names or indices
#' @examples
#' cars.data <- Dataset(data = cars, target = "dist")
#' cars.data[c(1, 2, 3, 4), ]
#' cars.data[c(1, 2), "dist"]
#' @export
`[.Dataset` <- function(x, i, j) {
  # Input checks
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
#' @param row.names A vector of row names
#' @param optional A logical value. If TRUE, setting row names and converting column names is optional.
#' @param ... Additional arguments
#' @param columns Which columns to include in the data frame: "all", "target", "features". By default "all".
#' 
#' @examples
#' cars.data <- Dataset(data = cars, target = "dist")
#' head(as.data.frame(cars.data))
#' @export
as.data.frame.Dataset <- function(x, row.names = NULL, optional = FALSE, ..., columns = "all") {
  # Input checks
  assertChoice(columns, c("all", "target", "features"))
  
  switch(columns,
         "all" = x$data,
         "target" = x$data[, x$target, drop = FALSE],
         "features" = x$data[, setdiff(names(x$data), x$target), drop = FALSE])
}

# Maybe we move the following to the metainfo file (?)
# Define the 'metainfo' generic function
metainfo <- function(x) {UseMethod("metainfo")}
#' @title 'metainfo' method for Dataset objects
#' 
#' @description Get the metainfo of a Dataset object (name, features, targets, nrow, type, missings)
#' 
#' @param x A Dataset object
#' @examples
#' cars.data <- Dataset(data = cars, target = "dist")
#' metainfo(cars.data)
#' @export
metainfo.Dataset <- function(x) {
  get_class <- function(x) {
    switch (class(x),
      numeric = "num",
      character = "char",
      logical = "logi",
      "other"
    )
  }
  
  feature_names <- setdiff(names(x$data), x$target)
  features <- get_class(x$data[, feature_names])
  names(features) <- feature_names
  
  info <- list(
    name = x$name,
    features = features,
    targets = setNames(get_class(x$data[[x$target]]), x$target),
    nrow = nrow(x$data),
    type = x$type,
    missings = any(is.na(x$data))
  )
  class(info) <- "DatasetInfo"
  return(info)
}

