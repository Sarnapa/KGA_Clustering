source("metrics.R")

fitnessFun <- function(dataset, solution) {
  clusteringMetrics <- getClusteringMetrics(dataset, solution)
  rating <- 1 / clusteringMetrics
  
  rating
}