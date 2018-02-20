#' Tests that the shapley algorithm converges.
#'
#' @description Tests that the shapley algorithm converges.
#' @param row.nr Index for the observation of interest.
#' @param iterations Amount of iterations.
#' @param shapley.iterations Amount of the iterations within the shapley
#'   function.
#' @return shapley value as a data.frame with col.names and their corresponding
#'   effects.
test.convergence = function(row.nr, iterations = 20, shapley.iterations = 1) {
  values = rep(0, times = iterations)
  for(i in 1:iterations) {
    values[i] = sum(shapley(row.nr, iterations = shapley.iterations))
  }

  return(values)
}

#a = test.convergence(196, iterations = 20)
#a.cum = cumsum(a) / seq_along(a)
#a.sub = a
#ggplot() +
#  geom_point(aes(x = seq_along(a), y = a)) +
#  geom_line(aes(x = seq_along(a), y = cumsum(a)/seq_along(a)), color = "red")
