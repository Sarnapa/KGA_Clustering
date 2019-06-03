initCenters <- function(dataset, initSize) {
  datasetId = dataset$id
  datasetAttrsCount = dataset$attrsCount
  datasetExamplesCount = dataset$examplesCount
  datasetFirstAttrIdx = dataset$firstAttrIdx
  
  if (missing(initSize)) {
    initSize <- floor(runif(1, min = 1, max = datasetExamplesCount))
  }
  
  centers <- matrix(nrow = initSize, ncol = datasetAttrsCount)
  
  if (datasetId == datasets()$IRIS$id) {
    if(!exists("iris"))
      data(iris)

    for(i in 1:initSize) {
      idx <- datasetFirstAttrIdx
      for(j in 1:datasetAttrsCount) {
        centers[i, j] = runif(1, min = min(iris[, idx]), max = max(iris[, idx]))
        idx <- idx + 1
      }
    }
  }
  else if (datasetId == datasets()$LETTER_RECOGNITION$id) {
    if(!exists("LetterRecognition"))
      data(LetterRecognition)
    
    for(i in 1:initSize) {
      idx <- datasetFirstAttrIdx
      for(j in 1:datasetAttrsCount) {
        centers[i, j] = round(runif(1, min = min(LetterRecognition[, idx]), max = max(LetterRecognition[, idx])))
        idx <- idx + 1 
      }
    }
  }
  
  centers
}