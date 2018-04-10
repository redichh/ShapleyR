#' Tests that the shapley algorithm converges.
#'
#' @description Tests that the shapley algorithm converges.
#' @param row.nr Index for the observation of interest.
#' @param convergence.iterations Amount of calls of the shapley function.
#' @param iterations Amount of the iterations within the shapley function.
#' @param return.value You can choose between plotting results or getting a data frame
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
  prediction.class = names(prediction[match(max(prediction), prediction)])

  values = rep(0, times = convergence.iterations)
  for(i in 1:convergence.iterations) {
    shap = shapley(row.nr, task, model, iterations)
    if(getTaskType(task) == "classif"){
      shap.sum = rowSums(shap[,getTaskFeatureNames(task)])
      values[i] = shap$"_Class"[match(max(shap.sum), shap.sum)]
    }
    else if(getTaskType(task) == "regr"){
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
      values = replace(values, grep(prediction.class, values), paste0(">>", prediction.class, "<<"))
      result = as.data.frame(values)
      names(result) = "values"
      result = summary(result$values)
    }
    else if(getTaskType(task) == "regr"){
      result = as.data.frame(values)
    }
    return(result)
  }
}
