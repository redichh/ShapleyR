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
#' @return A shapley object as a list containing several information. Among others the
#' shapley.values are returned as a data.frame with the features as columns and
#' their corresponding effects.
#' @export
shapley = function(row.nr, task = bh.task, model = train(makeLearner("regr.lm"), bh.task), iterations = 100) {
  assert_numeric(row.nr, min.len = 1, lower = 1, upper = nrow(getTaskData(task)))
  assert_int(iterations, lower = 1)
  assert_class(model, "WrappedModel")
  assert_set_equal(getTaskId(task), getTaskId(model))

  feature.names = getTaskFeatureNames(task)
  task.type = getTaskType(task)

  x = getTaskData(task)[row.nr,]
  b1 = data.frame(subset(x, select = feature.names))
  b1[1:(nrow(x) * iterations * length(feature.names)),] = NA
  b2 = b1
  result = prepareResult(x, task, model)

  for(f in 1:length(feature.names)) {
    feature = feature.names[f]
    for(i in 1:iterations) {
      z = getTaskData(task)[sample(getTaskSize(task), nrow(x)),]
      perm = sample(feature.names)
      position = match(feature, perm)

      s = (f-1) * nrow(x)*iterations + (i-1) * nrow(x) + 1
      prec = if(position == 1) NULL else perm[1:(position - 1)]
      succ = if(position == length(perm)) NULL else perm[(position + 1):length(perm)]
      b1[s:(s + nrow(x) - 1), perm] = cbind(x[prec], x[feature], z[succ])
      b2[s:(s + nrow(x) - 1), perm] = cbind(x[prec], z[feature], z[succ])
    }
  }

  predict_b1 = getPredictionData(b1, model, task)
  predict_b2 = getPredictionData(b2, model, task)
  nclasses = 1
  if(task.type %in% c("classif", "multilabel"))
    nclasses = length(getTaskClassLevels(task))
  if(task.type == "cluster")
    nclasses = model$learner$par.vals$centers

  for(f in 1:length(feature.names)) {
    feature = feature.names[f]
    for(i in 1:nrow(x)) {
      r.indices = nclasses * (i - 1) + 1:nclasses
      p.indices = custom.ifelse(getLearnerPredictType(model$learner) == "response",
        seq((f-1) * nrow(x) * iterations + i, f * nrow(x) * iterations, nrow(x)),
        iterations * (i-1) + 1:iterations)
      result[r.indices, feature] = computePartialResult(predict_b1, predict_b2, p.indices, task.type)
    }
  }

  result = list(
    task.type = task.type,
    feature.names = getTaskFeatureNames(task),
    predict.type = getLearnerPredictType(model$learner),
    prediction.response = getPredictionResponse(predict(model, newdata = getTaskData(task)[row.nr,])),
    data.mean = if(task.type == "regr")
      mean(getPredictionTruth(predict(model, newdata = getTaskData(task)))) else NA,
    values = result
  )

  return(result)
}

getPredictionData = function(data, model, task) {
  result = NA
  response.types = c("classif", "cluster", "multilabel")
  if(getLearnerPredictType(model$learner) == "response" &  getTaskType(task) %in% response.types) {
    task.levels = custom.ifelse(getTaskType(task) == "cluster",
      seq(1, model$learner$par.vals$centers, by = 1), getTaskClassLevels(task))
    response = getPredictionResponse(predict(model, newdata=data))
    estimated = as.data.frame(matrix(data = 1, nrow = 1, ncol = length(task.levels)))
    observed = as.data.frame(matrix(data = 0, nrow = nrow(data), ncol = length(task.levels)))
    names(estimated) = task.levels
    names(observed) = task.levels
    for(i in 1:nrow(observed)) {
      col.indices = custom.ifelse(getTaskType(task) == "multilabel",
        colnames(response)[response[i,]], as.character(response[i]))
      estimated[1, col.indices] = estimated[1, col.indices] - (1 / nrow(data))
      observed[i, col.indices] = 1
    }
    result = observed
    for(i in 1:nrow(observed))
      result[i,] = observed[i,] - estimated[1,]
  } else if(getTaskType(task) == "regr") {
    result = as.data.frame(getPredictionResponse(predict(model, newdata=data)))
  } else if(getTaskType(task) == "classif") {
    result = getPredictionProbabilities(predict(model, newdata=data), getTaskClassLevels(task))
  } else if(getTaskType(task) == "cluster") {
    result = getPredictionProbabilities(predict(model, newdata=data))
  }

  return(result)
}

prepareResult = function(x, task, model) {
  task.levels = NA
  if(getTaskType(task) %in% c("classif", "multilabel")) {
    task.levels = getTaskClassLevels(task)
  } else if(getTaskType(task) == "cluster") {
    task.levels = seq(1, model$learner$par.vals$centers)
  }
  custom.names = c("_Id", "_Class")
  result = as.data.frame(matrix(data = 0, ncol = getTaskNFeats(task) + length(custom.names),
    nrow = nrow(x) * length(task.levels)))
  names(result) = c(custom.names, getTaskFeatureNames(task))
  for(i in 1:nrow(x)) {
    s = (i - 1) * length(task.levels) + 1
    result$"_Id"[s:(s+length(task.levels)-1)] = rep(row.names(x)[i], times = length(task.levels))
  }
  result$"_Class" = rep(task.levels, times = nrow(x))

  return(result)
}

computePartialResult = function(predict_a, predict_b, indices, task_type) {
  result = predict_a[indices,] - predict_b[indices,]
  if(task_type == "regr")
    result = round(mean(result), 3)
  else
    result = round(colMeans(result), 3)

  return(result)
}

custom.ifelse = function(condition, then.case, else.case) {
  if(condition)
    return(then.case)
  else
    return(else.case)
}
