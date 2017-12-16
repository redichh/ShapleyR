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
shapley = function(row.nr, task = bh.task, learner = "regr.lm",
  iterations = 50, method = "default") {

  #FIXME: assert_factor(method, levels=c("default", "kernel"))
  #FIXME: remove at least one loop with vectorized operations (e.g. apply) see
  # branch hredich_parallel_phi_20171212
  mod = train(learner, task)
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

plot.shapley.singleValue = function(row.nr, shap.values, target = "medv",
  task = bh.task, learner = "regr.lm") {

  mod = train(learner, task)
  pred =
    getPredictionResponse(predict(mod, newdata = getTaskData(bh.task)[row.nr,]))

  points = compute.shapley.positions(shap.values, pred)
  ggplot(points, aes(x = values, y = 0, colour = values)) +
    geom_line(size = 4) +
    coord_cartesian(ylim = c(-0.4, 0.4)) +
    scale_colour_gradientn(colours=rainbow(4)) +
    geom_text(aes(label=names), size = 4, vjust = "top", hjust = points$align,
      colour = "black", angle = 20) +
    geom_point(aes(x = round(obs$medv, 3), y = 0.1), colour = "black", size = 3) +
    theme(axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none")
}

compute.shapley.positions = function(points, shift) {
  points.minus = -1 * sort(points[which(points < 0)])
  points.plus  = -1 * sort(points[which(points >= 0)], decreasing = TRUE)

  for(i in 1:length(points.minus))
    if(i > 1)
      points.minus[i] = points.minus[i-1] + points.minus[i]
  for(i in 1:length(points.plus))
    if(i > 1)
      points.plus[i] = points.plus[i-1] + points.plus[i]
  positions = sort(cbind(points.minus, 0, points.plus))
  result = data.frame(cbind(names(positions), t(round(positions + shift, 3))))
  names(result) = c("names", "values")
  result$values = sapply(sapply(result$values, as.character), as.numeric)
  result$values = result$values
  result$align = ifelse(result$values > shift,"right", "left")
  rownames(result) = c()

  return(result)
}








