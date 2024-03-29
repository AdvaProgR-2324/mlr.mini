% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Inducer.R
\name{copy.Inducer}
\alias{copy.Inducer}
\title{Create copy of an inducer.}
\usage{
\method{copy}{Inducer}(.inducer, new_configuration)
}
\arguments{
\item{.inducer}{S3 object of class \code{Inducer}.}

\item{new_configuration}{Named List, containing the hyperparameter
configuration.}
}
\value{
A new instance of class \code{Inducer} with new configuration values. If
no new configurations are passed, the original inducer is returned.
}
\description{
This function creates a copy of the passed inducer with new configuration values.
New configuration values which have not been passed during
initialization are added to the copy, while already present configuration
values are overwritten in the copy.
}
\examples{
InducerXgboost <- InducerConstructer(configuration = list(nrounds = 100),
                                   method = "Xgboost")
NewInducerXgboost <- copy(InducerXgboost,
                          new_configuration = list(nrounds = 20, max_depth = 6))
print(NewInducerXgboost)
NewInducerXgboost <- copy(InducerXgboost)
print(NewInducerXgboost)
}
