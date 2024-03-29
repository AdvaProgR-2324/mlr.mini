% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Resampling.R
\name{resample}
\alias{resample}
\title{'resample' method for SplitCV objects}
\usage{
resample(data, inducer, split_cv)
}
\arguments{
\item{data}{A Dataset object}

\item{inducer}{An Inducer Object}

\item{split_cv}{The split method or a SplitInstance object}
}
\description{
method that creates a ResamplePrediction. It contain the relevant
result information of a resampling: the predictions made, the relevant ground
truth values, and the training tasks and trained model for each fold
}
\examples{
cars.data <- Dataset(data = cars, target = "dist")
xgb <- InducerConstructer(configuration = list(nrounds = 10, verbose = 0), method = "XGBoost")
cars.split <- splt$cv(folds = 5)(cars.data)
resample(cars.data, xgb, cars.split)
}
