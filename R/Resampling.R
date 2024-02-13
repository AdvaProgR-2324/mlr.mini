#' @title Resampling S3 Class
#' 
#' @description
#' Split the data into a training and a validation set

#' @param data A Dataset object containing the data
#' @examples
#' cars.data <- Dataset(data = cars, target = "dist")
#' class(cars.data)
#' @export
#' 
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
#' @description Split the data into 2 parts: the training and validation part
#' 
#' @param folds A numeric value: give the number of folds
#' @param repeats A numeric value: the number of repetition
#' @examples
#' cars.data <- Dataset(data = cars, target = "dist")
#' cars.data[c(1, 2, 3, 4), ]
#' cars.data[c(1, 2), "dist"]
#' @export
SplitCV <- function(folds, repeats = 1){
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

#' set the class of the SplitCV object
class(SplitCV) <- c("SplitConstructorCV", "SplitConstructor", "function")

#' Create an environment for storing Split objects
splt <- new.env()

#' store the SplitCV object cv to the environment
splt$cv <- SplitCV


#' @title 'print' method for SplitCV objects
#' 
#' @description Print a SplitCV object
#' 
#' @param x A SplitInstance objec
#' @param ... Additional arguments
#' @examples
#' cars.split <- cv5(cars.data)
#' print(cars.data)
#' @export
#' 
print.SplitInstance <- function(x){
  
  # number of rows in the dataset
  rows <- length(x[[1]]$validation) + length(x[[1]]$training)
  
  folds <- rows / length(x[[1]]$training)
  repeats <- length(x) / folds
  
  name <- deparse(substitute(x))
  
  # Split the string by dot and extract he name
  name <- strsplit(name, "\\.")[[1]][1]

  cat("CV Split Instance of the \"", name, ", dataset (", rows, " rows)\n", sep = "")
  cat("Configuration: folds = ", folds, ", repeats = ", repeats, "\n", sep = "")
  invisible(x)
}


##' @title 'hypaerparameters' generic function
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

#' @title 'hypermarameters' method for SplitCV objects
#' 
#' @description Get the hyperparameters information of a Split object
#' 
#' @param x A SplitCV/SplitConstructorCV object
#' @examples
#' hyperparameters(splt$cv)
#' @export
hyperparameters.SplitConstructorCV <- function(x){
  cat("Hyperparameter Space: \n")
  data.frame(name = c("folds", "repeats"),
                type = c("int", "int"),
                range = c("[2, Inf]", "[1, Inf]"))
}

