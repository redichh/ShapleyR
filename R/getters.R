.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the ShapleyR package!")
}
#' Getting informations from the result of the shapley function.
#' @description You get the information from the shapley function by using the get-functions.
#' @param getShapleyValues get shapley values
#' @param getShapleyIds get Id from task
#' @param getShapleyTaskType get task type
#' @param getShapleyPredictionType get prediction type of the model
#' @param getShapleyPredictionResponse get prediction response of the model
#' @param getShapleyFeatureNames get feature names
#' @param getShapleyDataMean get mean of data set
#' @param getShapleySubsetByResponseClass ????
#' @export

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
