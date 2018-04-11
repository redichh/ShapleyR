getShapleyValues = function(shapley.list, subset = NULL) {
  result = shapley.list$values
  if(!is.null(subset) & is.numeric(subset))
    result = result[subset,]

  return(result)
}

getShapleyIds = function(shapley.list) {
  return(getShapleyValues(shapley.list)$"_Id")
}

getShapleyTaskType = function(shapley.list) {
  return(shapley.list$task.type)
}

getShapleyPredictionType = function(shapley.list) {
  return(shapley.list$predict.type)
}

getShapleyPredictionResponse = function(shapley.list) {
  return(shapley.list$prediction.response)
}

getShapleyFeatureNames = function(shapley.list) {
  return(shapley.list$feature.names)
}

getShapleyDataMean = function(shapley.list) {
  return(shapley.list$data.mean)
}

getShapleySubsetByResponseClass = function(shap.list) {
  if(getShapleyTaskType(shap.list) == "regr")
    return(shap.list)
  shapley.values = getShapleyValues(shap.list)
  shap.list$values = shapley.values[shapley.values$"_Class" == getShapleyPredictionResponse(shap.list),]
  return(shap.list)
}
