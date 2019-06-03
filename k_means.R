library(mlbench)

source("datasets.R")

kMeans <- function(dataset, centers) {
  clusters <- NULL
  datasetId = dataset$id
  datasetAttrsCount = dataset$attrsCount
  datasetFirstAttrIdx = dataset$firstAttrIdx
  datasetLastAttrIdx = dataset$lastAttrIdx
  
  if (datasetId == datasets()$IRIS$id) {
    if(!exists("iris"))
      data(iris)
    #print(summary(iris))

    clusters <- kmeans(x = iris[,datasetFirstAttrIdx:datasetLastAttrIdx], centers = centers, iter.max = 1)
  }
  else if (datasetId == datasets()$LETTER_RECOGNITION$id) {
    if(!exists("LetterRecognition"))
      data(LetterRecognition)
    #print(summary(LetterRecognition))

    clusters <- kmeans(LetterRecognition[,datasetFirstAttrIdx:datasetLastAttrIdx], centers = centers, iter.max = 1)
  }
  
  if (!is.null(clusters))
  {
    clusters$centers
  }
}