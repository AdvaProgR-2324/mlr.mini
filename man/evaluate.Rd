% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluators.R
\name{evaluate}
\alias{evaluate}
\title{Generic Evaluation Function}
\usage{
evaluate(evaluator, .prediction, .dataset = NULL, .model = NULL, ...)
}
\arguments{
\item{evaluator}{The evaluator function to be used for performance evaluation,
chosen from the \code{evl} environment (e.g., \code{evl$mae}, \code{evl$rmse}).}

\item{.prediction}{The prediction object to be evaluated.}

\item{.dataset}{Optional; a dataset object required by some evaluators.}

\item{.model}{Optional; a model object that might be required for some evaluations.}

\item{...}{Additional arguments}
}
\value{
Evaluation result, which varies depending on the evaluator used.
}
\description{
Generic Evaluation Function
}
\examples{
\dontrun{
  prediction <- data.frame(prediction = c(1, 2, 3), truth = c(1, 2, 4))
  result <- evaluate(evl$mae, prediction)
  print(result)
}
}
