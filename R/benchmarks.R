#' Tests that the shapley algorithm converges.
#'
#' @description Tests that the shapley algorithm converges.
#' @param row.nr Index for the observation of interest.
#' @param iterations Amount of iterations.
#' @param shapley.iterations Amount of the iterations within the shapley
#'   function.
#' @return shapley value as a data.frame with col.names and their corresponding
#'   effects.
test.convergence = function(row.nr, task = bh.task, model = train(makeLearner("regr.lm"), bh.task),
                            iterations = 20, shapley.iterations = 20) {

  prediction = predict(model, newdata = getTaskData(task)[row.nr,])
  truth = getPredictionTruth(prediction)
  response = getPredictionResponse(prediction)

  values = rep(0, times = iterations)
  for(i in 1:iterations) {
    shap = shapley(row.nr, task, model, iterations = shapley.iterations)
    values[i] = sum(shap[,3:ncol(shap)]) + truth
  }

  plot = ggplot() +
    geom_point(aes(x = seq_along(values), y = values)) +
    geom_line(aes(x = seq_along(values), y = cumsum(values)/seq_along(values)), color = "red") +
    geom_line(aes(x = seq(1:iterations), y = rep(response, iterations)), color = "black")

  return(plot)
}
