#' Getting informations from the result of the shapley function.
#' @description You get the information from the shapley function by using the get-functions.
#' @param shapley.list A shapley object.
#' @param subset A vector that determines the rows.
#' @param summate Indicates whether the shapley values should be summed up or not. Default is 'FALSE'.
#' @export
getShapleyValues = function(shapley.list, subset = NULL, summate = FALSE) {
  result = shapley.list$values
  if(!is.null(subset) & is.numeric(subset))
    result = result[subset,]
  if(summate)
    result = rowSums(result[, getShapleyFeatureNames(shapley.list)])
  return(result)
}
#' Getting informations from the result of the shapley function.
#' @description You get the information from the shapley function by using the get-functions.
#' @param shapley.list A shapley object.
#' @export
getShapleyIds = function(shapley.list) {
  return(getShapleyValues(shapley.list)$"_Id")
}

#' Getting informations from the result of the shapley function.
#' @description You get the information from the shapley function by using the get-functions.
#' @param shapley.list get shapley values
#' @export
getShapleyTaskType = function(shapley.list) {
  return(shapley.list$task.type)
}

#' Getting informations from the result of the shapley function.
#' @description You get the information from the shapley function by using the get-functions.
#' @param shapley.list A shapley object.
#' @export
getShapleyPredictionType = function(shapley.list) {
  return(shapley.list$predict.type)
}

#' Getting informations from the result of the shapley function.
#' @description You get the information from the shapley function by using the get-functions.
#' @param shapley.list A shapley object.
#' @export
getShapleyPredictionResponse = function(shapley.list) {
  return(shapley.list$prediction.response)
}

#' Getting informations from the result of the shapley function.
#' @description You get the information from the shapley function by using the get-functions.
#' @param shapley.list get shapley values
#' @export
getShapleyFeatureNames = function(shapley.list) {
  return(shapley.list$feature.names)
}

#' Getting informations from the result of the shapley function.
#' @description You get the information from the shapley function by using the get-functions.
#' @param shapley.list A shapley object.
#' @export
getShapleyDataMean = function(shapley.list) {
  return(shapley.list$data.mean)
}

#' Getting informations from the result of the shapley function.
#' @description This getter-function takes a shapley object. Unless the task type is "regr" it
#'   returns a shapley object with the same list elements except for the shapley.values. These are
#'   subseted to the response class.
#' @param shapley.list A shapley object.
#' @export
getShapleySubsetByResponseClass = function(shapley.list) {
  if(getShapleyTaskType(shapley.list) == "regr")
    return(shapley.list)

  shapley.values = getShapleyValues(shapley.list)
  shapley.list$values = shapley.values[shapley.values$"_Class" == getShapleyPredictionResponse(shapley.list),]
  return(shapley.list)
}
