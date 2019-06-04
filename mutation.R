mutation <- function(solution, M, Mmin, Mmax, xmin, xmax){
  
  if(Mmax > M){
    R <- (M - Mmin) / (Mmax - Mmin)
  } else if (Mmin == Mmax){
    R <- 1
  }
  
  delta <- runif(1, -R, R)
  
  mutIdx = sample(1:length(solution),1)
  # mutate each attribute
  for(i in 1:(length(solution[mutIdx]))){
    xi <- solution[mutIdx,i]
    if(delta >= 0){
      xi <- xi + delta * (xmax - xi)
    } else {
      xi <- xi + delta * (xi - xmin)
    }
    solution[mutIdx,i] <- xi
  }
  
  solution
}