library(mlbench)

source("datasets.R")

getClusteringMetrics <- function(dataset, centers) {
  clusteringMetrics <- 0
  
  if (datasetId == datasets()$IRIS$id) {
    if(!exists("iris"))
      data(iris)
  }
  else if (datasetId == datasets()$LETTER_RECOGNITION$id) {
    if(!exists("LetterRecognition"))
      data(LetterRecognition)
  }
}