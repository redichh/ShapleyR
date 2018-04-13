# ShapleyR
`shapleyR` is an R package that provides some functionality to use `mlr` tasks and models to generate shapley values. And thus analyze the effects of the features on the outcome of a model.
`shapleyR` already supports the regression, classification, clustering and multilabel tasks from `mlr`. We plan to add the missing tasks from that package.

The package can be installed directly from github with devtools (see following section). Beside that we also plan to upload this package to CRAN as soon as it gets production ready.

## Installation
```
> install.packages("devtools")
> devtools::install_github('redichh/shapleyR')
> library(shapleyR)
```

## Quickstart

As a quickstart we will calculate the shapley values for a regression task. For that we take a look at the Boston Housing dataset. This is alredy included in the mlr-package and can be called with `bh.task` from the R terminal.
The Dataset looks as following:

```
> head(getTaskData(bh.task))
crim zn indus chas   nox    rm  age    dis rad tax ptratio      b lstat medv
0.00632 18  2.31    0 0.538 6.575 65.2 4.0900   1 296    15.3 396.90  4.98 24.0
0.02731  0  7.07    0 0.469 6.421 78.9 4.9671   2 242    17.8 396.90  9.14 21.6
0.02729  0  7.07    0 0.469 7.185 61.1 4.9671   2 242    17.8 392.83  4.03 34.7
0.03237  0  2.18    0 0.458 6.998 45.8 6.0622   3 222    18.7 394.63  2.94 33.4
0.06905  0  2.18    0 0.458 7.147 54.2 6.0622   3 222    18.7 396.90  5.33 36.2
0.02985  0  2.18    0 0.458 6.430 58.7 6.0622   3 222    18.7 394.12  5.21 28.7
```

Wheras the first 13 features are the describing variables and the last one "medv" is the dependant variable. The prediction for the `medv` feature is:
```
> prediction = head(getPredictionResponse(predict(train("regr.lm", bh.task), newdata = getTaskData(bh.task))))
[1] 30.00384 25.02556 30.56760 28.60704 27.94352 25.25628
```

And the mean for `medv` over all observation available in the dataset is:
```
> data.mean = mean(getTaskData(bh.task)[,getTaskTargetNames(bh.task)])
[1] 22.53281
```

Now we can take a look at the shapley-function itself. Running following code shows the influence for every feature according to a specific observation:
```
> shap.values = getShapleyValues(shapley(1:6, task = bh.task, model = train("regr.lm", bh.task)))
  _Id _Class  crim     zn  indus   chas   nox     rm    age    dis    rad   tax ptratio     b lstat
1   1     NA 0.275  0.256 -0.185 -0.358 1.070  1.632 -0.002  0.065 -2.510 1.229   2.690 0.337 4.468
2   2     NA 0.503 -0.385 -0.078 -0.358 2.026 -0.014  0.005 -2.157 -1.969 2.419   0.407 0.455 2.233
3   3     NA 0.389 -0.244 -0.090 -0.269 1.542  3.346 -0.008 -1.740 -2.724 2.139   0.213 0.227 4.538
4   4     NA 0.363 -0.390 -0.152 -0.090 1.673  2.048 -0.014 -3.148 -1.938 1.746  -0.254 0.318 4.894
5   5     NA 0.427 -0.666 -0.167 -0.448 1.840  3.286 -0.012 -3.435 -3.060 2.852   0.079 0.450 4.004
6   6     NA 0.454 -0.430 -0.165  0.000 1.957  1.211 -0.010 -3.336 -1.367 2.455  -0.143 0.666 3.907
```

Taking the sum of all explaining features for every row results in the following:
```
> approximation = rowSums(shap.values[,getTaskFeatureNames(bh.task)])
[1] 8.967 3.087 7.319 5.056 5.150 5.199
```

And this is the approximated difference between the previously calculated `prediction` and `data.mean`. Assuming that the sum of all shapley values for one observation equals the difference between the prediction and the data mean the following calculation should be close to zero:
```
> prediction - data.mean - approximation
[1] -1.4959629 -0.5942439  0.7157904  1.0182302  0.2607179 -2.4755219
```

We see that this is not the case. But increasing the amount of iterations should lead to better results:

```
> shap.values.2 = getShapleyValues(shapley(1:6, task = bh.task, model = train("regr.lm", bh.task)), iterations = 200)
> approximation.2 = rowSums(shap.values[,getTaskFeatureNames(bh.task)])
> prediction - data.mean - approximation.2
[1] -0.1209629  0.7367561 -0.3352096 -0.1327698 -0.1692821 -0.1395219
```

## Further information
In our Vignette can be found further information about this package. There is also shown the usage of plots for the shapley values.
