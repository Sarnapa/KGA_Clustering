crossover <- function(parents){
  p1 <- parents[[1]]
  p2 <- parents[[2]]
  
  print(p1)
  print(p2)
  
  len <- length(p1)
  
  crossoverPoint = sample(1:(len-1), 1)
  tmp <- p1[crossoverPoint:len]
  p1[crossoverPoint:len] <- p2[crossoverPoint:len]
  p2[crossoverPoint:len] <- tmp
  list(p1, p2)
}