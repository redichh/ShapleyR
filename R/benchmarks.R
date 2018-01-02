# internal functions to verify that the algorith works and coverges
test.covergence = function(iterations = 20) {
  values = rep(0, times = iterations)
  for(i in 1:iterations) {
    values[i] = sum(shapley(2, iterations = 1))
    if(i %% 100 == 0)
      print(i)
  }

  return(values)
}
