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
#' @param learner Learner or String that determines the mlr learning algorithm.
#' @param iterations Amount of iterations.
#' @param method Determines how the shapley value is calculated. Possible
#'   selections are "default" or "kernel".
#' @return shapley value as a data.frame with col.names and their corresponding
#'   effects.
#' @export
shapley <- function(row.nr, task = bh.task, learner = "regr.lm",
  iterations = 50, method = "default") {

  #FIXME: assert_factor(method, levels=c("default", "kernel"))
  #FIXME: remove at least one loop with vectorized operations (e.g. apply) see
  # branch hredich_parallel_phi_20171212
  if(class(learner) == "WrappedModel") {
    mod = learner
  } else {
    mod = train(learner, task)
  }
  x = getTaskData(task)[row.nr,]
  phi = as.data.frame(matrix(data = 0, nrow = 1, ncol = getTaskNFeats(task)))
  names(phi) = getTaskFeatureNames(task)

  for(feature in getTaskFeatureNames(task)) {
    for(i in 1:iterations) {
      z = getTaskData(task)[sample(getTaskSize(task), 1),]
      perm = sample(getTaskFeatureNames(task))
      position = match(feature, perm)

      b1 = cbind(x[perm[1:position]], z[perm[min((position + 1), length(perm)):length(perm)]])
      b2 = cbind(x[perm[1:max(1, (position - 1))]], z[perm[position:length(perm)]])

      phi[feature] = phi[feature] +
        getPredictionResponse(predict(mod, newdata = b1)) -
        getPredictionResponse(predict(mod, newdata = b2))
    }
  }

  return(phi / iterations)
}
