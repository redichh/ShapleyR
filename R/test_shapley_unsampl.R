#Shapley value calculatet by shapley.calc
#Iterates over all player
shapley.unsampled = function(data.input = test, target = "value") {
  #assert()
  #df is a list without target variable
  df = data.input[, !(names(data.input) %in% target)]
  #Find all permutations of players
  S = combinat::permn(names(df))

  shapley.calc = function(observed, S){
    sh.diff = c()
    for(i in 1:length(S)){
      index = grep(pattern = observed, x=S[[i]])
      #Choose players before/with observed player, No coalation = NA
      player.with = c(S[[i]][1:index])
      if(index!=1) player.before = c(S[[i]][1:(index-1)])
      else player.before = NA
      #Target value of coalations
      for(j in 1:nrow(df)){
        curr.coal = c(names(df)[df[j,] == 1])
        if(length(curr.coal)==length(player.with) & all(sort(curr.coal)==sort(player.with))){
          target.with = data.input$value[j]
        }
        if(is.na(player.before)){
          target.before = 0
        }
        if(length(curr.coal)==length(player.before) & all(sort(curr.coal)==sort(player.before))){
          target.before = data.input$value[j]
        }
      }
      #Substract the value of coalations
      sh.diff = append(sh.diff, target.with - target.before)
    }
    return(mean(sh.diff))
  }
  ##iterate observed player
  sh.all = c()
  for (observed in names(df)){
    shapley.value = shapley.calc(observed = observed, S = S)
    sh.all = append(sh.all, paste(observed, shapley.value))
  }
  return(sh.all)
}
print(sh.all)

#######
#######Second, older version.No iteration through player. No shapley.calc function.
sh = c()
shapley.unsampled = function(data.input = test, target = "value",  observed = "B") {
  #assert()
  #df is a list without target variable
  df = data.input[, !(names(data.input) %in% target)]
  #Find all permutations of players
  S = combinat::permn(names(df))
  for(i in 1:length(S)){
    index = grep(pattern = observed, x=S[[i]])
    #Choose players before/with observed player, No coalation = NA
    player.with = c(S[[i]][1:index])
    if(index!=1) player.before = c(S[[i]][1:(index-1)])
    else player.before = NA
    #Target value of coalations
    for(j in 1:nrow(df)){
      curr.coal = c(names(df)[df[j,] == 1])
      if(length(curr.coal)==length(player.with) & all(sort(curr.coal)==sort(player.with))){
        target.with = data.input$value[j]
      }
      if(is.na(player.before)){
        target.before = 0
      }
      if(length(curr.coal)==length(player.before) & all(sort(curr.coal)==sort(player.before))){
        target.before = data.input$value[j]
      }
    }
    #Substract the value of coalations
    sh = append(sh, target.with - target.before)
  }
  return(mean(sh))
}
