# Mutation operator
mutation <- function(solution, xmin, xmax, limitCentersCount, isInteger){
  
  newCentersCount <- floor(runif(1, 1, (limitCentersCount + 1)))
  oldCentersCount <- nrow(solution)
  attrsCount <- ncol(solution)
  lowerCount <- 0
  greaterCount <- 0
  
  if (newCentersCount >= oldCentersCount)  {
    lowerCount = oldCentersCount
  }
  else {
    lowerCount = newCentersCount
  }
  
  for(i in 1:lowerCount) {
    for(j in 1:attrsCount)
    {
      xi <- solution[i, j]
      delta <- runif(1, -1, 1)
      
      if(delta >= 0) {
        xi <- xi + delta * (xmax[j] - xi)
      } else {
        xi <- xi + delta * (xi - xmin[j])
      }
      if (isInteger) {
        solution[i, j] <- round(xi)
      }
      else {
        solution[i, j] <- xi
      }
    }
  }
  
  if (newCentersCount > oldCentersCount) {
    for (i in (oldCentersCount + 1):newCentersCount) {
      newCentres <- vector("numeric", attrsCount)
      for (j in 1:attrsCount)
      {
        if (isInteger) {
          newCentres[j] <- round(runif(1, xmin[j], xmax[j])) 
        }
        else {
          newCentres[j] <- runif(1, xmin[j], xmax[j])
        }
      }
      solution <- rbind(solution, setNames(newCentres, names(newCentres)))
    }
  }
  else if (newCentersCount < oldCentersCount) {
    solution <- solution[-(newCentersCount + 1):-oldCentersCount, , drop=F]
  }
  
  solution
}