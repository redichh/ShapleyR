#' @description This method draws a plot for, the observed value
#'   and describes the influence of features we interested .
#' @param row.nr Index for the observations of interest.
#' @param features the features we observations
#' @param shap.values (Optional) A data.frame that contains the shapley values. If
#'   no shap.values are given the default algorithm is used to calculate the
#'   shapley values for the given observation.
#' @param target The name of the dependent variable.
#' @param task mlr task that contains the data set.
#' @param learner Learner or String that determines the mlr learning algorithm.
#' @export
#' @example plot.shapleyFeatures.multiplies(1:200,features = c("crim","zn","indus","chas"))
plot.shapleyFeatures.multiplies = function(row.nr, features = c("crim", "lstat"), shap.values = NULL, target = "medv",
  task = bh.task, learner = "regr.lm") {
  if (is.null(shap.values))
    shap.values = shapley(row.nr)

  features.values = shap.values[features]
  features.numbers = ncol(features.values)

  data = data.frame(matrix(data = 0, nrow = length(row.nr), ncol = 1+features.numbers))
  names(data) = c(names(features.values), "position")
  data[names(features.values)] = features.values
  data$position = row.nr


  plot.data = melt(data, id.vars = "position")

  ggplot(data=plot.data, aes(x = position, y = value, group = variable, color = variable))+
    geom_line()


}
