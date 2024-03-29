% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Models.R
\name{predict.ModelXGBoost}
\alias{predict.ModelXGBoost}
\title{'predict' method for \code{ModelXGBoost}}
\usage{
\method{predict}{ModelXGBoost}(object, newdata, type = "response", ...)
}
\arguments{
\item{object}{A model object}

\item{newdata}{A data frame or Dataset with the new data}

\item{type}{A character string specifying the type of prediction: "response", "se" (standard error), or "prob" (probability). The default is "response".}

\item{...}{Additional arguments}
}
\description{
this function predicts the response variable using a trained ModelXGBoost object
}
\examples{
cars.data <- Dataset(data = cars, target = "dist")
xgb <- InducerConstructer(configuration = list(nrounds = 10, verbose = 0), method = "XGBoost")
model.xgb <- fit(xgb, cars.data)
predict(model.xgb, newdata = data.frame(speed = 10))
prediction <- predict(model.xgb, newdata = cars.data[c(1, 2, 3, 4), ])
prediction
}
