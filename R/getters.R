getShapleyValues = function(shapley.list, subset = NULL) {
  result = shapley.list$values
  if(!is.null(subset) & is.numeric(subset))
    result = result[subset,]

  return(result)
}

getShapleyTaskType = function(shapley.list) {
  return(shapley.list$task.type)
}

getShapleyPredictionType = function(shapley.list) {
  return(shapley.list$predict.type)
}

getShapleyPredictionResponse = function(shap.values) {
  return(shap.values$prediction.response)
}

getShapleyFeatureNames = function(shap.values) {
  return(shap.values$feature.names)
}

getShapleyDataMean = function(shap.values) {
  return(shap.values$data.mean)
}
