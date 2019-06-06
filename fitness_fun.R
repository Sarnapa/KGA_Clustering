source("metrics.R")

fitnessFun <- function(dataset, solution, alfa, xvar) {
  g <- getGFactor(dataset, solution, alfa, xvar)

  rating <- 1 / g
  
  rating
}