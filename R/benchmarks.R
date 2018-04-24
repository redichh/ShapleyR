#' Tests that the shapley algorithm converges.
#'
#' @description Tests that the sum of values from the shapley algorithm converges against the
#'   difference of the mean of data and the prediction for the given observation. Supported tasks are
#'   classification, clustering and regression. Note that only for regression it is possible to choose
#'   to plot the result. For classification and clustering tasks, the predicted class (chosen with
#'   row.nr) is marked like this: >>ClassName<< . The result should show that this class was chosen most often.
#' @param row.nr Index for one observation of interest (not possible to choose a range of rows).
#' Input must be numeric.
#' @param convergence.iterations Amount of calls of the shapley function. Input must be one numeric.
#' @param iterations Amount of the iterations within the shapley function. Input must be one numeric.
#' @param return.value You can choose between plotting results or getting a data frame
#' with "plot" or "values".
#' @return shapley values as a data.frame or a plot
#'
convergence.shapley = function(row.nr, convergence.iterations = 30, iterations = 50, task = bh.task,
  model = train(makeLearner("regr.lm"), bh.task), return.value = "plot") {

  assert_number(convergence.iterations)
  assert_number(row.nr) #hier keine vektoren zulassen
  assert_number(iterations)
  assert_choice(return.value, c("plot", "values"))
  assert_class(model, "WrappedModel")

  data = getTaskData(task)[row.nr,]
  if(getTaskType(task) == "classif"){
    prediction = getPredictionData(data, model, task)
    prediction.class = names(prediction[match(max(prediction), prediction)])
  }
  if(getTaskType(task) == "cluster"){
    prediction = predict(model, newdata = data)
    prediction.response = getPredictionResponse(prediction)
  }

  values = rep(0, times = convergence.iterations)
  if(getTaskType(task) == "cluster"){
    for(i in 1:convergence.iterations){
      shap = shapley(row.nr, task, model, iterations)
      shap.sum = rowSums(shap$values[,getTaskFeatureNames(task)])
      values[i] = shap$values$"_Class"[match(min(shap.sum), shap.sum)]
    }
  } else if(getTaskType(task) == "classif"){
    for(i in 1:convergence.iterations){
      shap = shapley(row.nr, task, model, iterations)
      shap.sum = rowSums(shap$values[,getTaskFeatureNames(task)])
      values[i] = shap$values$"_Class"[match(max(shap.sum), shap.sum)]
    }
  } else if(getTaskType(task) == "regr"){
    for(i in 1:convergence.iterations){
      shap = shapley(row.nr, task, model, iterations)
      prediction = predict(model, newdata = data)
      response = getPredictionResponse(prediction)
      data.mean = mean(getTaskData(task)[, getTaskTargetNames(task)])
      values[i] = sum(shap$values[,getTaskFeatureNames(task)]) + data.mean
    }
  }

  if(getTaskType(task) == "regr" && return.value == "plot") {
    plot = ggplot() +
      geom_point(aes(x = seq_along(values), y = values, colour = "Sum of shapley values")) +
      geom_line(aes(x = seq_along(values), y = cumsum(values)/seq_along(values), colour = "Moving Average")) +
      geom_line(aes(x = seq(1:convergence.iterations), y = rep(response, convergence.iterations), colour = "Response")) +
      scale_colour_discrete(name = NULL) +
      labs(x = "Convergence iterations", y = "Shapley value") +
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
    else if(getTaskType(task) == "cluster"){
      values = replace(values, grep(prediction.response, values), paste0(">>", prediction.response, "<<"))
      result = as.data.frame(values)
      names(result) = "values"
      result = summary(as.factor(result$values))
    }
    else if(getTaskType(task) == "regr"){
      result = as.data.frame(values)
    }

    return(result)
  }
}
