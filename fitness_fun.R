source("metrics.R")

# Defined fitness function calculation
fitnessFun <- function(dataset, solution, alfa, xvar) {
  g <- getGFactor(dataset, solution, alfa, xvar)

  rating <- 1 / g
  
  rating
}