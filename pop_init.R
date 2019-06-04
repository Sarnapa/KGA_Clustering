initPop <- function(dataset, popSize, maxCentersCount) {
  pop <- list()
  if (missing(maxCentersCount) | maxCentersCount < 0) {
    maxCentersCount <- dataset$examplesCount
  }
  
  for(i in 1:popSize) {
    centersCount = floor(runif(1, min = 1, max = maxCentersCount))
    pop[[i]] = initIndividual(dataset, centersCount)
  }
    
  pop
}

initIndividual <- function(dataset, centersCount) {
  datasetId = dataset$id
  datasetAttrsCount = dataset$attrsCount
  datasetExamplesCount = dataset$examplesCount
  datasetFirstAttrIdx = dataset$firstAttrIdx
  
  if (missing(centersCount) | centersCount < 0) {
    centersCount <- floor(runif(1, min = 1, max = datasetExamplesCount))
  }
  
  centers <- matrix(nrow = centersCount, ncol = datasetAttrsCount)
  
  if (datasetId == datasets()$IRIS$id) {
    if(!exists("iris"))
      data(iris)

    for(i in 1:centersCount) {
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
    
    for(i in 1:centersCount) {
      idx <- datasetFirstAttrIdx
      for(j in 1:datasetAttrsCount) {
        centers[i, j] = round(runif(1, min = min(LetterRecognition[, idx]), max = max(LetterRecognition[, idx])))
        idx <- idx + 1 
      }
    }
  }
  
  centers
}