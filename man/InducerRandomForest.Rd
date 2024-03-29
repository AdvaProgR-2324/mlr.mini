% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/InducerRandomForest.R
\name{InducerRandomForest}
\alias{InducerRandomForest}
\title{Construct or fit a Random Forest Inducer.}
\usage{
InducerRandomForest(.data, ...)
}
\arguments{
\item{.data}{Object of class \code{Dataset}. Optional.}

\item{...}{Additional parameters specifying the hyperparameter configuration
of the Random Forest model. If none are given, the default configuration of
\code{randomForest::randomForest} is used for the model fitting process.}
}
\value{
A S3 object of either class \code{Model} (\code{.data} was given) or of class
\code{InducerRandomForest} (\code{.data} was not given).
}
\description{
Construct or fit a Random Forest Inducer.
}
\details{
If \code{.data} argument is passed, an Random Forest model will be trained on the given
dataset using the passed hyperparameter configuration. Otherwise, an inducer of
class \code{InducerRandomForest} is created with the given hyperparameter configuration.
}
