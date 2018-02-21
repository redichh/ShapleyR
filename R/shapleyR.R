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
shapley = function(row.nr, model = train("regr.lm", bh.task), task = bh.task,
  iterations = 50, method = "default") {

  #FIXME: add version with unsampled permutation for small feature vectors
  #FIXME: add further methods = c("default", "kernel", "exact", "harsanyi-dividends"))
  #FIXME: test/implement further task kinds (classification, clustering)
  #FIXME: add "#' @importFrom mlr train" for methods

  assert_numeric(row.nr, min.len = 1, lower = 1, upper = nrow(getTaskData(task)))
  assert_int(iterations, lower = 1)
  assert_class(model, "WrappedModel")


  x = getTaskData(task)[row.nr,]
  phi = as.data.frame(matrix(data = 0, nrow = nrow(x) * iterations, ncol = getTaskNFeats(task)))
  names(phi) = getTaskFeatureNames(task)

  b1 = data.frame(subset(x, select = names(phi)))
  b1[1:(nrow(x) * iterations),] = NA
  b2 = b1

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

    phi[feature] = getPredictionResponse(predict(model, newdata=b1)) -
      getPredictionResponse(predict(model, newdata = b2))
  }

  result = as.data.frame(matrix(data=0, ncol=ncol(phi), nrow=nrow(x)))
  names(result) = names(b1)
  for(i in 1:nrow(x))
    result[i,] = colMeans(phi[seq(from=i, to=nrow(phi), by=nrow(x)), ])

  return(round(result, 3))
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
shapley.classif = function(row.nr, model = train("classif.lda", iris.task), task = iris.task,
  iterations = 50, method = "default") {

  assert_numeric(row.nr, min.len = 1, lower = 1, upper = nrow(getTaskData(task)))
  assert_int(iterations, lower = 1)
  assert_class(model, "WrappedModel")

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

    if(model$learner$type == "classif") {
      class.levels = getTaskDesc(task)$class.levels
      phi = phi[1:length(class.levels),]
      row.names(phi) = class.levels
      predict_b1 = getPredictionResponse(predict(model, newdata=b1))
      predict_b2 = getPredictionResponse(predict(model, newdata=b2))
      for(class.level in class.levels)
        phi[class.level, feature] = mean(ifelse(predict_b1 == class.level, 1, 0))
      #for(class.level in class.levels)
      x.class = as.character(x[getTaskDesc(task)$target][,1])
      result[feature] = 2 * phi[x.class, feature] - sum(phi[,feature])
    } else {
      phi[feature] = getPredictionResponse(predict(model, newdata=b1)) -
        getPredictionResponse(predict(model, newdata = b2))
      for(k in 1:nrow(x))
        result[k,] = colMeans(phi[seq(from=k, to=nrow(phi), by=nrow(x)), ])
    }
  }

  return(round(result, 3))
}
