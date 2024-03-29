% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Models.R
\name{modelObject}
\alias{modelObject}
\title{Model Object}
\usage{
modelObject(model)
}
\arguments{
\item{model}{A model object}
}
\description{
this function gets the object generated by its inducer e.g. lm, xgboost::xgboost, randomForest::randomForest
}
\examples{
cars.data <- Dataset(data = cars, target = "dist")
xgb <- InducerConstructer(configuration = list(nrounds = 10, verbose = 0), method = "XGBoost")
model.xgb <- fit(xgb, cars.data)
modelObject(model.xgb)
}
