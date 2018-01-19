# internal functions to verify that the algorith works and coverges
test.covergence = function(row.nr, iterations = 20, shapley.iterations = 1) {
  values = rep(0, times = iterations)
  for(i in 1:iterations) {
    values[i] = sum(shapley(row.nr, iterations = shapley.iterations))
    if(i %% 100 == 0)
      print(i)
  }

  return(values)
}

a = test.covergence(196, iterations = 20)
a.cum = cumsum(a) / seq_along(a)
a.sub = a
ggplot() +
  geom_point(aes(x = seq_along(a), y = a)) +
  geom_line(aes(x = seq_along(a), y = cumsum(a)/seq_along(a)), color = "red")

#system.time({ shapley(1:196, iterations = 30)})


## Für Daniel:
#for(i in 1:length(vec)) {
#  print(combn(vec, i))
#}
