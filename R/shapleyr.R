.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the ShapleyR package!")
}

#' Describes the difference between the expected value and the true outcome.
#'
#' @description Takes an observation, the corresponding dataset where x is from
#'   and the algorithm that was applied to the dataset. With this input the
#'   shapley value for all features is calculated.
#' @param x A single observation of interest.
#' @param data The dataset that contains x.
#' @param algoirthm The algorithm that computes the expected value/class of x.
#' @param method Determines how the shapley value is calculated. Possible
#'   selections are "default" or "regression".
#' @return shapley value as a matrix with col.names and their corresponding
#'   effects.
#' @export
shapley <- function(x, data, algorithm, method = "default") {

  return(x)
}
