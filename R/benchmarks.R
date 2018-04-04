#' Tests that the shapley algorithm converges.
#'
#' @description Tests that the shapley algorithm converges.
#' @param row.nr Index for the observation of interest.
#' @param convergence.iterations Amount of calls of the shapley function.
#' @param iterations Amount of the iterations within the shapley function.
#' @return shapley values as a data.frame or a plot
#'
test.convergence = function(row.nr=2, convergence.iterations = 20, iterations = 20, task = iris.task,
                            model = train(makeLearner("classif.lda", predict.type = "prob"), iris.task),
                            return.value = "values") {

  assert_number(iterations, convergence.iterations, row.nr)
  assert_choice(return.value, c("plot", "values"))
  assert_class(model, "WrappedModel")

  data = getTaskData(task)[row.nr,]
  prediction = getPredictionData(data, model, task)
  prediction.class = names(prediction[BBmisc::getMaxIndex(prediction)])

  values = rep(0, times = convergence.iterations)
  for(i in 1:convergence.iterations) {
    if(getTaskType(task) == "classif"){
      shap = shapley(row.nr, task, model, iterations)
      values[i] = shap$"_Class"[BBmisc::getMaxIndex(rowSums(shap[,getTaskFeatureNames(task)]))]
    }
    else if(getTaskType(task) == "regr"){
      shap = shapley(row.nr, task, model, iterations)
      truth = getPredictionTruth(prediction)
      values[i] = sum(shap[,getTaskFeatureNames(task)]) + truth
    }
  }
  if(return.value == "plot") {
    plot = ggplot() +
      geom_point(aes(x = seq_along(values), y = values, colour = "Sum of Shapley values")) +
      geom_line(aes(x = seq_along(values), y = cumsum(values)/seq_along(values), colour = "Moving Averages")) +
      geom_line(aes(x = seq(1:convergence.iterations), y = rep(response, convergence.iterations), colour = "Prediction")) +
      scale_colour_discrete(name = NULL) + labs(x = "Convergence iterations", y = "Shapley value") +
      theme(legend.position="bottom")
    return(plot)
  }
  if(return.value == "values") {
    if(getTaskType(task) == "classif"){
      result = plyr::count(as.data.frame(values))
      result$values = as.character(result$values)
      result$values[match(prediction.class, result$values)] = paste0(">>", prediction.class, "<<")
    }
    else if(getTaskType(task) == "regr"){
      result = as.data.frame(values)
    }
    return(result)
  }
}
