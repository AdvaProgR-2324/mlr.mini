# mlr.mini

![r-cmd-check](https://github.com/AdvaProgR-2324/mlr.mini/actions/workflows/r-cmd-check.yml/badge.svg)

[package website](https://advaprogr-2324.github.io/mlr.mini/)

## Overwiew

`mlr.mini` is a lightweight R package for making machine learning easier. The primary goal is to provide a unified interface for various machine learning algorithms, making the experience smoother across different models. The package includes models like XGBoost, RandomForest, and general linear models. It also includes tools for performance evaluation and resampling.

## Installation

You can install the package from GitHub using the following command:

``` r
devtools::install_github("advaprogr-2324/mlr.mini")
```

## Usage

Here is a basic example of how to use `mlr.mini`:

### Train a model

Construct an Inducer carrying some hyperparameters and than fit it:

```r
ind <- InducerXGBoost(verbose = 0, nrounds = 10)
configuration(ind)
```

Or fit the data set directly:

``` r
cars.data <- Dataset(data = cars, target = "dist")
model.xgb <- InducerXGBoost(cars.data, verbose = 0, nrounds = 10)
```

### Make predictions

``` r
predict(model.xgb, newdata = data.frame(speed = 10))
```

### Evaluate the model

``` r
cv5 <- splt$cv(folds = 5)
cars.split <- cv5(cars.data)
rp <- resample(cars.data, xgb, cars.split)
mae <- evl$mae
mae(rp)
```
