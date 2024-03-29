% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Models.R
\name{inducer}
\alias{inducer}
\title{Inducer Information}
\usage{
inducer(model)
}
\arguments{
\item{model}{A model object}
}
\description{
this functions returns the inducer and the configuration of a model
}
\examples{
cars.data <- Dataset(data = cars, target = "dist")
xgb <- InducerConstructer(configuration = list(nrounds = 10, verbose = 0), method = "XGBoost")
model.xgb <- fit(xgb, cars.data)
inducer(model.xgb)
}
