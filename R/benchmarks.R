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

a = test.covergence(196, iterations = 60)
a.cum = cumsum(a) / seq_along(a)
a.sub = a
ggplot() +
  geom_point(aes(x = seq_along(a), y = a)) +
  geom_line(aes(x = seq_along(a), y = cumsum(a)/seq_along(a)), color = "red")

a[5000]
(cumsum(a) / seq_along(a))[4999]
mean(a[60])


# test what does the shapley really return...
test.shapley = function() {
  #do smthng
}
s.1 = shapley(196, iterations = 250)
s.2 = shapley2(196, iterations = 250)

system.time({ shapley(196, iterations = 1000)})
system.time({ shapley2(196, iterations = 1000)})


## Für Daniel:
for(i in 1:length(vec)) {
  print(combn(vec, i))
}
