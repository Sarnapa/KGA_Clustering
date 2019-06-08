# Population initializaion
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

# Single individual initialization
initIndividual <- function(dataset, centersCount) {
  data <- NULL
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
    data <- iris
  }
  else if (datasetId == datasets()$LETTER_RECOGNITION$id) {
    if(!exists("LetterRecognition"))
      data(LetterRecognition)
    data <- LetterRecognition
  }
  
  for(i in 1:centersCount) {
    idx <- datasetFirstAttrIdx
    for(j in 1:datasetAttrsCount) {
      centers[i, j] = runif(1, min = min(data[, idx]), max = max(data[, idx]))
      idx <- idx + 1
    }
  }
  
  centers
}