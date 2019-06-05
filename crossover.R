crossover <- function(parents){
  p1 <- parents[[1]]
  p2 <- parents[[2]]
  
  # print("parents")
  # print(p1)
  # print(p2)
  
  p1len <- length(p1[,1])
  p2len <- length(p2[,1])
  crossoverPointP1 = 1
  crossoverPointP2 = 1

  if (p1len != 1 | p2len != 1)
  {
    if (p1len < p2len) {
      if (p1len == 1) {
        crossoverPointP2 = sample(1:(p2len), 1)
      }
      else {
        crossoverPoint = sample(1:(p1len-1), 1) 
        crossoverPointP1 = crossoverPoint
        crossoverPointP2 = crossoverPoint
      }
    } else {
      if (p2len == 1) {
        crossoverPointP1 = sample(1:(p1len), 1) 
      }
      else {
        crossoverPoint = sample(1:(p2len-1), 1)
        crossoverPointP1 = crossoverPoint
        crossoverPointP2 = crossoverPoint
      }
    }
    # print("crossoverPoint")
    # print(crossoverPoint)
    
    if (crossoverPointP1 != 1 || crossoverPointP2 != 1) {
      p1tail <- p1[crossoverPointP1:p1len, ,drop=F]
      p2tail <- p2[crossoverPointP2:p2len, ,drop=F]
      # print("tails")
      # print(p1tail)
      # print(p2tail)
      
      if (crossoverPointP1 == 1) {
        p1 <- p2tail 
      }
      else {
        p1 <- rbind(p1[1:(crossoverPointP1-1),], p2tail)
      }
      
      if (crossoverPointP2 == 1) {
        p2 <- p1tail
      }
      else {
        p2 <- rbind(p2[1:(crossoverPointP2-1),], p1tail)
      }
    }
  }
  
  list(p1, p2)
}