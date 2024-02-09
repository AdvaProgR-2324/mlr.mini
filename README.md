# mlr.mini
[package website](https://advaprogr-2324.github.io/mlr.mini/)
## Overwiew

`mlr.mini` is a lightweight R package for making machine learning easier. The primary goal is to provide a unified interface for various machine learning algorithms, making the experience smoother across different models. The package includes models like XGBoost, RandomForest, and linear models.

## Usage

Here is a basic example of how to use `mlr.mini` to train a model and make predictions.

``` r
cars.data <- Dataset(data = cars, target = "dist")
xgb <- InducerConstructer(configuration = list(nrounds = 10, verbose = 0), method = "XGBoost")
model.xgb <- fit(xgb, cars.data)
predict(model.xgb, newdata = data.frame(speed = 10))
```
