crossover <- function(parents){
  p1 <- parents[[1]]
  p2 <- parents[[2]]
  
  # print("parents")
  # print(p1)
  # print(p2)
  
  p1len <- length(p1[,1])
  p2len <- length(p2[,1])
  
  if(p1len < p2len){
    crossoverPoint = sample(1:(p1len-1), 1)
  } else {
    crossoverPoint = sample(1:(p2len-1), 1)
  }
  # print("crossoverPoint")
  # print(crossoverPoint)
  
  p1tail <- p1[crossoverPoint:p1len,]
  p2tail <- p2[crossoverPoint:p2len,]
  # print("tails")
  # print(p1tail)
  # print(p2tail)
  
  p1 <- rbind(p1[1:(crossoverPoint-1),], p2tail)
  p2 <- rbind(p2[1:(crossoverPoint-1),], p1tail)
  
  list(p1, p2)
}