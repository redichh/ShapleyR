# ShapleyR
Package for a nice and smoothe usage of the shapley value for mlr
`shapleyR` is an R package that provides some functionality to use `mlr` tasks and models to generate shapley values. And thus analyze the effects of the features on the outcome of a model.
`shapleyR` already supports the regression, classification, clustering and multilabel tasks from `mlr`. We plan to add the missing tasks from that package.

The package can be installed directly from github with devtools (see following section). Beside that we also plan to upload this package to CRAN as soon as it gets production ready.

## Installation
```
install.packages("devtools")
devtools::install_github('redichh/shapleyR')
library(shapleyR)
```

## Quickstart

As a quickstart we will calculate the shapley values for a regression task. For that we take a look at the Boston Housing dataset. This is alredy included in the mlr-package and can be called with `bh.task`.
