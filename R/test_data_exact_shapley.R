test = as.data.frame(rbind(c(1,0,0,12),
  c(0,1,0,6),
  c(0,0,1,9),
  c(1,1,0,24),
  c(1,0,1,27),
  c(0,1,1,15),
  c(0,0,0,0),
  c(1,1,1,36)))

names(test) = c("Ha", "He", "Da", "value")

test.task = makeRegrTask(id = "wiki-example", data = test, target = "value")
