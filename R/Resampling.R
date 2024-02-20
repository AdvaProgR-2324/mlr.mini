library(checkmate)
source("R/Dataset.R")
source("R/Inducer.R")
source("R/InducerXgboost.R")
source("R/Models.R")

#' @title Resampling S3 Class
#' 
#' @description
#' Split the data into a training and a validation set
#' 
#' @param data A Dataset object containing the data
#' @examples
#' cars.data <- Dataset(data = cars, target = "dist")
#' str(Split(cars.data))
#' @export
Split <- function(data){
  # Assertion
  assertClass(data, "Dataset")
  
  # extract the name of the data
  name <- data$name
  # convert the data in a dataframe
  data <- as.data.frame(data)
  # the number of row that the data contain
  nrow <- nrow(data)
  
  structure(list(data = data, 
                 name = name,
                 nrow = nrow),
            class = c("Split"))
}


#' @title SplitCV method for splitting the data
#' 
#' @description Create a function that splits the data into 2 parts: the training and validation part
#' 
#' @param folds A numeric value: give the number of folds
#' @param repeats A numeric value: the number of repetition
#' @examples
#' cv3 <- SplitCV(folds = 3, repeats = 2)
#' cars.data <- Dataset(data = cars, target = "dist")
#' cars.split <- cv3(cars.data)
#' @export
SplitCV <- function(folds, repeats = 1) {
  # Input checks
  assertNumeric(folds)
  assertNumeric(repeats)
  
  # define the split function
  Spl <- function(data){
  
    x <- Split(data)
    
    nbr_row <- x$nrow
    
    # the train indices
    n_train <- nbr_row/folds
    # the validation indices
    n_validation <- nbr_row - n_train
    
    # the list of all splits
    split_indices_list <- list()
    
    for(r in 1:repeats){
      
      fold_indices <- rep(1:folds, length.out = nbr_row)
      split_indices <- split(sample(nbr_row, size = nbr_row, replace = FALSE), fold_indices)
      
     for(i in 1:folds) {
      
      training <- split_indices[[i]]
      validation <- setdiff(seq_len(nbr_row), training)
      split_indices_list[[paste0("repeat_", r, "_fold_", i)]] <- list(training = training,
                          validation = validation)
     }
    }
    
    structure(split_indices_list,
    class = c("SplitInstanceCV", "SplitInstance")
    )
  }
  class(Spl) = c("SplitCV", "Split")
  return(Spl)
}

#' @name set_class_SplitCV
#' @title set the class of the SplitCV object
class(SplitCV) <- c("SplitConstructorCV", "SplitConstructor", "function")

#' Create an environment for storing Split objects
#' @export
splt <- new.env()

#' @name store_SplitCV_in_environment
#' @title the SplitCV object cv to the environment
splt$cv <- SplitCV


#' @title 'print' method for SplitInstance objects
#' 
#' @description Print a SplitInstance object
#' 
#' @param x A SplitInstance object
#' @param ... Additional arguments
#' @examples
#' cars.data <- Dataset(data = cars, target = "dist")
#' cv5 <- splt$cv(folds = 5)
#' cars.split <- cv5(cars.data)
#' print(cars.split)
#' @export
print.SplitInstance <- function(x, ...){
  
  # number of rows in the dataset
  rows <- length(x[[1]]$validation) + length(x[[1]]$training)
  
  folds <- rows / length(x[[1]]$training)
  repeats <- length(x) / folds
  
  name <- deparse(substitute(x))
  
  # Split the string by dot and extract he name
  name <- strsplit(name, "\\.")[[1]][1]

  cat("CV Split Instance of the \"", name, "\" dataset (", rows, " rows)\n", sep = "")
  cat("Configuration: folds = ", folds, ", repeats = ", repeats, "\n", sep = "")
  invisible(x)
}

#' @title splitInstantiate
#' 
#' @description Split the data into a training and a validation set
#' 
#' @param split A Split object
#' @param data A Dataset object containing the data
#' @examples
#' cars.data <- Dataset(data = cars, target = "dist")
#' cv5 <- splt$cv(folds = 5)
#' cars.split <- splitInstantiate(cv5, cars.data)
#' @export
splitInstantiate <- function(split, data){
  assertClass(split, "Split")
  assertClass(data, "Dataset")
  
  split(data)
}

#' @title 'configuration' method for `Split` objects
#' 
#' @description Get the configuration of a Split object
#' 
#' @param x A Split object
#' @examples
#' cv5 <- splt$cv(folds = 5)
#' configuration(cv5)
#' @export
configuration.Split <- function(x) {
  list(folds = environment(x)$folds, repeats = environment(x)$repeats)
}


#' @title 'hyperparameters' generic function
#' 
#' @description Get information about the hyperparameters of an object
#' 
#' @param x An object
#' @examples
#' hyperparameters(splt$cv)
#' @export
hyperparameters <- function(x){
  UseMethod("hyperparameters")
}

#' @title 'hyperparameters' method for SplitCV objects
#' 
#' @description Get the hyperparameters information of a Split object
#' 
#' @param x a SplitCV/SplitConstructorCV object
#' @examples
#' hyperparameters(splt$cv)
#' @export
hyperparameters.SplitConstructorCV <- function(x){
  cat("Hyperparameter Space: \n")
  data.frame(name = c("folds", "repeats"),
                type = c("int", "int"),
                range = c("[2, Inf]", "[1, Inf]"))
}


#' @title 'resample' method for SplitCV objects
#' 
#' @description method that creates a ResamplePrediction. It contain the relevant 
#' result information of a resampling: the predictions made, the relevant ground 
#' truth values, and the training tasks and trained model for each fold
#' 
#' @param data A Dataset object
#' @param inducer An Inducer Object
#' @param split_cv The split method or a SplitInstance object
#' @examples
#' cars.data <- Dataset(data = cars, target = "dist")
#' xgb <- InducerConstructer(configuration = list(nrounds = 10, verbose = 0), method = "XGBoost")
#' cars.split <- splt$cv(folds = 5)(cars.data)
#' resample(cars.data, xgb, cars.split)
#' @export
resample <- function(data, inducer, split_cv) {
  assertClass(data, "Dataset")
  assertClass(inducer, "Inducer")
  assertMultiClass(split_cv, c("SplitInstance", "Split"))
  
  if ("Split" %in% class(split_cv)) {
    split_cv <- split_cv(data)
  }
  
  results <- list()
  
  for (split in names(split_cv)) {
    train_data <- data[split_cv[[split]]$train, ]
    validation_data <- data[split_cv[[split]]$validation, ]
    
    # Apply inducer to the training data
    model <- fit(inducer, train_data)
    
    # Evaluate the model on the validation data
    # You need to replace this part with your evaluation code
    # For example, you might use predict() and some performance metric
    validation_predictions <- predict(model, newdata = validation_data)
    
    # Store the results
    results[[split]] <- list(
      model = model,
      predictions = validation_predictions
    )
  }
  structure(results, class = "ResamplePrediction")
}

#' @title 'print' method for ResamplePrediction objects
#' 
#' @description Print a ResamplePrediction object
#' 
#' @param x A ResamplePrediction object
#' @param ... Additional arguments
#' @examples
#' cars.data <- Dataset(data = cars, target = "dist")
#' xgb <- InducerConstructer(configuration = list(nrounds = 10, verbose = 0), method = "XGBoost")
#' cars.split <- splt$cv(folds = 5)(cars.data)
#' rp <- resample(cars.data, xgb, cars.split)
#' print(rp)
#' @export
print.ResamplePrediction <- function(x, ...){
  repeat_indices <- unique(gsub(".*repeat_(\\d+)fold\\d+", "\\1", names(x)))
  n_repeats <- length(repeat_indices)
  n_folds <- length(x) / n_repeats
  
  cat("ResamplePrediction Object:\n")
  cat("- Number of Repeats: ", n_repeats, "\n", sep = "")
  cat("- Number of Folds: ", n_folds, "\n", sep = "")
  cat("- ", x[[1]]$model$task," Model: \"", x[[1]]$model$inducer$method, "\" fitted on \"", x[[1]]$model$data$name, "\" dataset\n", sep = "")
  invisible(x)
}