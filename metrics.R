library(mlbench)

source("datasets.R")

getClusteringMetrics <- function(dataset, centers) {
  clusteringMetrics <- 0
  data <- NULL
  datasetId = dataset$id
  datasetAttrsCount = dataset$attrsCount
  datasetFirstAttrIdx = dataset$firstAttrIdx
  datasetExamplesCount = dataset$examplesCount
  
  if (datasetId == datasets()$IRIS$id) {
    if(!exists("iris"))
      data(iris)
    data <- iris
  }
  else if (datasetId == datasets()$LETTER_RECOGNITION$id) {
    if(!exists("LetterRecognition"))
      data(LetterRecognition)
    data <- LetterRecognition
  }

  centersCount <- nrow(centers)
    
  for(i in 1:datasetExamplesCount)
  {
    centerDist <- -1

    for(j in 1:centersCount) {
      center <- centers[j, ,drop = FALSE]
      centerDistTmp <- getMinDistance(data, datasetFirstAttrIdx, datasetAttrsCount, i, center)
      if (centerDist < 0 | centerDistTmp < centerDist) {
        centerDist <- centerDistTmp
      }
    }
    clusteringMetrics <- clusteringMetrics + centerDist
  }
  
  clusteringMetrics
}

getMinDistance <- function(dataset, datasetFirstAttrIdx, datasetAttrsCount, exampleIdx, center) {
  centerDist <- 0
  idx <- datasetFirstAttrIdx
  
  for (i in 1:datasetAttrsCount) {
    centerDist <- centerDist + (center[i] - dataset[exampleIdx, idx])^2
    idx <- idx + 1
  } 
  
  centerDist
}