#' Plots the difference for a single value.
#'
#' @description This method draws a plot for the data.mean, the observed value
#'   and describes the influence of features/variables for this difference.
#' @param row.nr Index for the observation of interest.
#' @param shap.values (Optional) A data.frame that contains the shapley values. If
#'   no shap.values are given the default algorithm is used to calculate the
#'   shapley values for the given observation.
#' @param target The name of the dependent variable.
#' @param task mlr task that contains the data set.
#' @param learner Learner or String that determines the mlr learning algorithm.
#' @export
plot.shapley.singleValue = function(row.nr, shap.values = NULL, target = "medv",
  task = bh.task, learner = "regr.lm") {

  if (is.null(shap.values))
    shap.values = shapley(row.nr)

  mod = train(learner, task)
  pred =
    getPredictionResponse(predict(mod, newdata = getTaskData(task)[row.nr,]))
  data.mean = mean(getPredictionTruth(predict(mod, newdata = getTaskData(task))))

  points = compute.shapley.positions(shap.values, pred)
  ggplot(points, aes(x = values, y = 0, colour = values)) +
    geom_line(size = 4) +
    coord_cartesian(ylim = c(-0.4, 0.4)) +
    scale_colour_gradientn(colours=rainbow(4)) +
    geom_text_repel(aes(label=names), colour = "black") +
    #    size = 4, vjust = "top", hjust = points$align, angle = 20) +
    geom_point(aes(x = round(data.mean, 3), y = 0.1), colour = "black", size = 3) +
    theme(axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none")
}

#' Plots a graph with the expected and observed values.
#'
#' @description This method draws a plot for the data.mean, the observed value
#'   and describes the influence of features/variables for this difference.
#' @param row.nr Index for the observations of interest.
#' @param shap.values (Optional) A data.frame that contains the shapley values. If
#'   no shap.values are given the default algorithm is used to calculate the
#'   shapley values for the given observation.
#' @param target The name of the dependent variable.
#' @param task mlr task that contains the data set.
#' @param learner Learner or String that determines the mlr learning algorithm.
#' @export
plot.shapley.multipleValues = function(row.nr, shap.values = NULL, target = "medv",
  task = bh.task, learner = "regr.lm") {

  if (is.null(shap.values))
    shap.values = shapley(row.nr)

  data = data.frame(matrix(data = 0, nrow = length(row.nr), ncol = 4))
  names(data) = c("truth", "response", "position", "color")
  data$truth = getTaskData(task)[row.nr, target]
  data$response = rowSums(shap.values) + data$truth
  data$position = row.nr
  data$color = ifelse(data$truth > data$response, "red", "green")

  #FIXME: change color acording to what line is below/above
  #FIXME: add useful legend
  ggplot() +
    geom_ribbon(data = data, aes(x = position, ymax = truth, ymin = response),
      fill = "blue", alpha = .1) +
    geom_line(data = data, aes(x = position, y = truth, colour = "expected")) +
    geom_line(data = data, aes(x = position, y = response, colour = "difference"))
}

#' Calculates the positions of the features influence for the plot.singleValue.
#'
#' @description This method draws a plot for the data.mean, the observed value
#'   and describes the influence of features/variables for this difference.
#' @param row.nr Index for the observations of interest.
#' @param shap.values (Optional) A data.frame that contains the shapley values. If
#'   no shap.values are given the default algorithm is used to calculate the
#'   shapley values for the given observation.
#' @param target The name of the dependent variable.
#' @param task mlr task that contains the data set.
#' @param learner Learner or String that determines the mlr learning algorithm.
compute.shapley.positions = function(points, shift) {
  points.minus = cumsum(-1 * sort(points[which(points < 0)]))
  points.plus  = cumsum(-1 * sort(points[which(points >= 0)], decreasing = TRUE))
  positions = sort(cbind(points.minus, 0, points.plus))

  result = data.frame(cbind(names(positions), t(round(positions + shift, 3))))
  names(result) = c("names", "values")
  result$values = sapply(sapply(result$values, as.character), as.numeric)
  result$align = ifelse(result$values > shift,"right", "left")
  rownames(result) = c()

  return(result)
}
