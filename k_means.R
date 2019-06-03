library(mlbench)

source("datasets.R")

kMeans <- function(dataset, centers) {

  clusters <- NULL
  if (dataset == datasetEnum()$IRIS) {
    data(iris)
    print(summary(iris))
    
    clusters <- kmeans(iris[,0:3], 3)
  }
  else if (dataset == datasetEnum()$LETTER_RECOGNITION) { 
    data(LetterRecognition)
    print(summary(LetterRecognition))
    
    clusters <- kmeans(LetterRecognition[,2:17], 26)
  }
  
  if (!is.null(clusters))
  {
    str(clusters)
  }
}