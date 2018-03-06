.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the ShapleyR package!")
}

#' Describes the difference between the expected value and the true outcome.
#'
#' @description Takes an observation, the corresponding dataset where x is from
#'   and the algorithm that was applied to the dataset. With this input the
#'   shapley value for all features is calculated.
#' @param row.nr Index for the observation of interest.
#' @param task mlr task that contains the data set.
#' @param model Model for the corresponding task..
#' @param iterations Amount of iterations.
#' @param method Determines how the shapley value is calculated. Possible
#'   methods are "default" or "kernel".
#' @return shapley value as a data.frame with col.names and their corresponding
#'   effects.
#' @export
shapley = function(row.nr, learner = makeLearner("regr.lm"), task = bh.task,
  model = train(learner, bh.task), iterations = 50, method = "regr") {

  #FIXME: implement further task kinds (clustering, decision trees)
  assert_numeric(row.nr, min.len = 1, lower = 1, upper = nrow(getTaskData(task)))
  assert_int(iterations, lower = 1)
  assert_class(model, "WrappedModel")

  result = NA
  if(method == "regr") {
    result = shapley.regr(row.nr, model, task, iterations)
  } else if (method == "classif") {
    result = shapley.classif(row.nr, learner, task, model, iterations)
  } else if (method == "unsampled") {
    print("Work in progress")
    # result = shapley.unsampled()
  } else {
    print(paste0("Method: '", method, "' not supported"))
  }

  return(result)
}


shapley.regr = function(row.nr, task = bh.task, model = train("regr.lm", bh.task),
  iterations = 50) {

  assert_set_equal(getTaskType(task), "regr")

  x = getTaskData(task)[row.nr,]
  phi = as.data.frame(matrix(data = 0, nrow = nrow(x) * iterations, ncol = getTaskNFeats(task)))
  names(phi) = getTaskFeatureNames(task)

  b1 = data.frame(subset(x, select = names(phi)))
  b1[1:(nrow(x) * iterations),] = NA
  b2 = b1

  result = as.data.frame(matrix(data=0, ncol=ncol(phi), nrow=nrow(x)))
  names(result) = names(b1)

  for(feature in getTaskFeatureNames(task)) {
    for(i in 1:iterations) {
      z = getTaskData(task)[sample(getTaskSize(task), nrow(x)),]
      perm = sample(getTaskFeatureNames(task))
      position = match(feature, perm)

      s = (i - 1) * nrow(x) + 1
      prec = if(position == 1) NULL else perm[1:(position - 1)]
      succ = if(position == length(perm)) NULL else perm[(position + 1):length(perm)]
      b1[s:(s + nrow(x) - 1), perm] = cbind(x[prec], x[feature], z[succ])
      b2[s:(s + nrow(x) - 1), perm] = cbind(x[prec], z[feature], z[succ])
    }

    predict_b1 = getPredictionResponse(predict(model, newdata=b1))
    predict_b2 = getPredictionResponse(predict(model, newdata=b2))
    for(i in 1:nrow(x))
      result[i, feature] = colMeans((predict_b1 - predict_b2)[seq(from=i, to=nrow(phi), by=nrow(x))])
  }

  return(round(result, 3))
}


shapley.classif = function(row.nr, learner = makeLearner("classif.lda", predict.type = "prob"),
  task = iris.task, model = train(learner, task), iterations = 50) {

  assert_set_equal(getTaskType(task), "classif")
  assert_set_equal(getLearnerPredictType(learner), "prob")

  x = getTaskData(task)[row.nr,]
  b1 = data.frame(subset(x, select = getTaskFeatureNames(task)))
  b1[1:(nrow(x) * iterations),] = NA
  b2 = b1

  task.levels = getTaskClassLevels(task)
  custom.names = c("_Id", "_Class")
  result = as.data.frame(matrix(data = 0, ncol = ncol(phi) + length(custom.names),
    nrow = nrow(x) * length(task.levels)))
  names(result) = c(custom.names, names(b1))
  result$"_Id" = sort(rep(row.nr, times = length(task.levels)))
  result$"_Class" = rep(task.levels, times = nrow(x))

  for(feature in getTaskFeatureNames(task)) {
    for(i in 1:iterations) {
      z = getTaskData(task)[sample(getTaskSize(task), nrow(x)),]
      perm = sample(getTaskFeatureNames(task))
      position = match(feature, perm)

      s = (i - 1) * nrow(x) + 1
      prec = if(position == 1) NULL else perm[1:(position - 1)]
      succ = if(position == length(perm)) NULL else perm[(position + 1):length(perm)]
      b1[s:(s + nrow(x) - 1), perm] = cbind(x[prec], x[feature], z[succ])
      b2[s:(s + nrow(x) - 1), perm] = cbind(x[prec], z[feature], z[succ])
    }

    predict_b1 = getPredictionProbabilities(predict(model, newdata=b1))
    predict_b2 = getPredictionProbabilities(predict(model, newdata=b2))
    nclasses = length(getTaskClassLevels(task))
    for(i in 1:nrow(x)) {
      r.indices = nclasses * (i - 1) + 1:nclasses
      p.indices = iterations * (i - 1) + 1:iterations
      result[r.indices, feature] =
        round(colMeans(predict_b1[p.indices,] - predict_b2[p.indices,]), 3)
    }
  }

  return(result)
}
