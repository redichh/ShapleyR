#' Plots the difference for a single value.
#'
#' @description This method draws a plot for the data.mean, the observed value
#'   and describes the influence of features/variables for this difference.
#' @param row.nr Index for the observation of interest.
#' @param shap.values (Optional) A data.frame that contains the shapley values. If
#'   no shap.values are given the default algorithm is used to calculate the
#'   shapley values for the given observation.
#' @param task mlr task that contains the data set.
#' @param model Model for the corresponding task.
#' @export
plot.shapley.singleValue = function(shap.values) {
  data.mean = getShapleyDataMean(shap.values)
  data = getShapleyValues(shap.values)[,getShapleyFeatureNames(shap.values)]
  points = compute.shapley.positions(data, data.mean)

  return(
    ggplot(points, aes(x = values, y = 0)) +
      coord_cartesian(ylim = c(-.4, .4)) +
      scale_colour_gradient2(low = "#832424FF", high = "#3A3A98FF", mid = "lightgrey", midpoint = data.mean) +
      geom_line(aes(colour = values), size = 30) +
      geom_text(aes(label = names), check_overlap = TRUE, angle = 70,
                nudge_y = rep(c(.1, -.1), times = nrow(points))[1:nrow(points)]) +
      geom_point(aes(x = round(getShapleyPredictionResponse(shap.values), 3), y = 0.1), colour = "black", size = 3) +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "none")
  )
}

#' Plots a graph with the expected and observed values.
#'
#' @description This method draws a plot for the data.mean, the observed value
#'   and describes the influence of features/variables for this difference.
#' @param row.nr Index for the observations of interest.
#' @param shap.values (Optional) A data.frame that contains the shapley values. If
#'   no shap.values are given the default algorithm is used to calculate the
#'   shapley values for the given observation.
#' @param task mlr task that contains the data set.
#' @param model Model for the corresponding task.
#' @export
plot.shapley.multipleValues = function(shap.values, task = bh.task, model = train("regr.lm", bh.task)) {
  data.names = c("response.plus", "response.minus", "position", "color")
  data = data.frame(matrix(data = 0, nrow = length(row.nr), ncol = length(data.names)))
  names(data) = data.names

  data.mean =
    mean(getPredictionTruth(predict(model, newdata = getTaskData(task))))
  data$response.plus = rowSums(apply(shap.values, 1:2, FUN = function(x) {max(0, x)}))
  data$response.minus = rowSums(apply(shap.values, 1:2, FUN = function(x) {min(0, x)}))
  data$position = row.nr
  data$color = ifelse(data$response.plus < abs(data$response.minus), "red", "green")

  ggplot() +
    geom_line(data = data, aes(x = position, y = data.mean, colour = "data mean")) +
    geom_line(data = data, aes(x = position, y = data.mean + response.plus,
                               colour = "positive effects")) +
    geom_line(data = data, aes(x = position, y = data.mean + response.minus,
                               colour = "negative effects")) +
    geom_ribbon(data = data, aes(x = position, ymax = data.mean,
                                 ymin = data.mean + rowSums(shap.values)), fill = "blue", alpha = .2)
}

#' Calculates the positions of the features influence for the plot.singleValue.
#'
#' @description Orders the values by their sign and value, shifts them and returns
#'   them as a vector.
#' @param points shapley.values
#' @param shift data.mean
compute.shapley.positions = function(points, shift = 0) {
  points.minus = sort(points[which(points < 0)])
  points.plus = sort(points[which(points >= 0)], decreasing = TRUE)
  points.labels = c(names(rev(points.minus)), "0", names(points.plus))
  positions = sort(c(cumsum(t(points.minus)), 0, cumsum(t(points.plus))))

  result = data.frame(positions + shift)
  names(result) = c("values")
  result$names = points.labels
  result$align = ifelse(result$values > shift,"right", "left")

  return(result)
}
