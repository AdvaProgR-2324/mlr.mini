# Constructer for Dataset()

# Dataset object

Dataset <- function(data, target, name){
  
  # if name is not given
  
  if(missing(name)) {
    name <- as.name(deparse(substitute(data), 20)[[1]])
  }
  
  if(!(target %in% colnames(data))) stop("data must contain the target")
  
  ind <- which(colnames(data) == target) ## columns indice of the target in the data
  targetcolumn <- data[, ind, drop = FALSE]
  
  # features 
  features <- colnames(data)[-ind]
  
  # data
  data <- data[, c(target, features), drop = FALSE]
  
  # number of row
  number_of_row <- nrow(data)
  
  if(is.numeric(targetcolumn[, 1])){
    cl <- "DatasetRegression"      ## Task
    type <- "regression"
  }
  else if(is.character(targetcolumn[, 1])){
    cl <- "DatasetClassification"         #Task
    type <- "classification"
  }
  
  # missings value
  if(any(is.na(data))){
    missings <- TRUE
  }else missings <- FALSE
  
  value <- list(name = name, 
                features = features,
                target = target,
                data = data,
                nrow = number_of_row,
                type = type,
                missings = missings)
  attr(value, "class") <- c(cl, "Dataset")
  value
}

#[-function

`[.Dataset` <- function(x, row, col){
  
  # if col is not given
  
  if(missing(col)){
    col <- colnames(x$data)
  }
  
  if(!(x$target %in% col)) stop("Error: Cannot remove target column 'dist'")
  
  result <- x$data[row, col, drop = FALSE]
  target <- x$target
  
  return(Dataset(result, target = target, name = x$name))
}


# the method as.data.frame.Dataset()


as.data.frame.Dataset <- function(x, column){
  
  
  if(missing(column)) {
    result <- x$data
  } 
  else if (column == "target") {
    result <- x[, x$target]
  } 
  else if(column == "covariates") {
    result <- x$data[, x$features, drop = FALSE]
  }
  
  result
}

x <- function(col){
  if(col == "x") result <- T
  else if(col == "y") result <- F
  result
}

## print method

print <- function(data){
  UseMethod("print")
}

print.Dataset <- function(obj){
  if("DatasetRegression" %in% class(obj)){
    task <- "Regression"
  }
  if("DatasetClassification" %in% class(obj)){
    task <- "Classification"
  }
  cat("Dataset \"", obj$name, "\", predicting \"", obj$target, "\" (", task,  ")\n", sep = "")
  
  #as.data.frame(obj)
  print(obj$data)
  
}

# generic metainfo()

metainfo <- function(x){
  UseMethod("metainfo")
}

## metainfo() method
metainfo.Dataset <- function(x){
  element <- list(name = x$name,
                  features = setNames(class(x$data[[x$features]]), x$features),
                  targets = setNames(class(x$data[[x$target]]), x$target),
                  nrow = x$nrow,
                  type = x$type,
                  missings = x$missings)
  attr(element, "class") <- "Datasetinfo"
  element
}


## Example
cars.data <- Dataset(data = cars, target = "dist")
print(cars.data)
class(cars.data)

cars.data[c(1, 2, 3, 4), ]
cars.data[c(1, 2), "dist"]


metainfo(cars.data)


as.data.frame(cars.data)
