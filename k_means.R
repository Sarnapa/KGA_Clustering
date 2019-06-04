library(mlbench)

source("datasets.R")

kMeans <- function(dataset, 
                   centers,
                   maxIter = 1) {
  clusters <- NULL
  data <- NULL
  datasetId = dataset$id
  datasetAttrsCount = dataset$attrsCount
  datasetFirstAttrIdx = dataset$firstAttrIdx
  datasetLastAttrIdx = dataset$lastAttrIdx
  
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
  
  clusters <- kmeans(x = data[,datasetFirstAttrIdx:datasetLastAttrIdx], centers = centers, iter.max = maxIter)
  
  if (!is.null(clusters))
  {
    clusters$centers
  }
}