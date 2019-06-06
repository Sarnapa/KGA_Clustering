library(mlbench)

source("datasets.R")

getGFactor <- function(dataset, centers, alfa, xvar) {
  g <- 0
  data <- NULL
  clustersCount <- nrow(centers)
  clustersExamples <- list()
  clustersVars <- list()
  for (i in 1:clustersCount) {
    clustersExamples[[i]] <- list()
    clustersVars[[i]] <- 0
  }
  
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
  
  for(i in 1:datasetExamplesCount)
  {
    centerIdx <- -1
    centerDist <- -1
    
    for(j in 1:clustersCount) {
      center <- centers[j, ,drop = FALSE]
      centerDistTmp <- getDistance(data, datasetFirstAttrIdx, datasetAttrsCount, i, center)
      if (centerDist < 0 | centerDistTmp < centerDist) {
        centerIdx <- j
        centerDist <- centerDistTmp
      }
    }
    clusterExamplesCount <- length(clustersExamples[[centerIdx]])
    clustersExamples[[centerIdx]][[clusterExamplesCount + 1]] <- list(i, centerDist)
  }
  
  for (i in 1:clustersCount) {
    clustersVars[[i]] <- getClusterVar(clustersExamples[[i]])
  }
  
  scat <- getScat(clustersVars, xvar)

  dis <- 0
  if (clustersCount > 1)
  {
    centersDists <- getCentersDists(centers)
    dmaxDmin <- getDmaxDmin(centersDists)
    dmax <- dmaxDmin$dmax
    dmin <- dmaxDmin$dmin
    dis <- getDis(centersDists, dmax, dmin)
  }
  
  print(scat)
  print(dis)
  
  g <- (alfa * scat) + dis
  
  g
}

getDistance <- function(dataset, datasetFirstAttrIdx, datasetAttrsCount, exampleIdx, x) {
  centerDist <- 0
  idx <- datasetFirstAttrIdx
  
  for (i in 1:datasetAttrsCount) {
    centerDist <- centerDist + (dataset[exampleIdx, idx] - x[i])^2
    idx <- idx + 1
  } 
  
  centerDist
}

getCentersDistance <- function(center1, center2) {
  centersDist <- 0
  attrsCount <- length(center1)
  
  for (i in 1:attrsCount) {
    centersDist <- centersDist + (center2[i] - center1[i])^2
  }
  
  centersDist
}

getClusterVar <- function(clusterExamples) {
  sum <- 0
  var <- 0
  n <- length(clusterExamples)
  if (n != 0) {
    for (i in 1:n) {
      sum <- sum + clusterExamples[[i]][[2]]
    }
      
    var <- sum / n
  }
  
  var
}

getVar <- function(dataset, xmean) {
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
  
  sum <- 0
  
  for (i in 1:datasetExamplesCount) {
    sum <- sum + getDistance(data, datasetFirstAttrIdx, datasetAttrsCount, i, xmean) 
  }
  
  var <- sum / datasetExamplesCount
  
  var
}

getScat <- function(clustersVars, xvar) {
  sum <- 0
  clustersCount <- length(clustersVars)
  for (i in 1:clustersCount) {
    sum <- sum + clustersVars[[i]]
  }
  
  scat <- sum / xvar
  
  scat
}

# It isn't necessary to count all values - some are duplicates
getCentersDists <- function(centers) {
  centersCount <- nrow(centers)

  centersDists <- matrix(data = 0L, nrow = centersCount, ncol = centersCount)
  if (centersCount > 1)
  {
    for (i in 1:(centersCount - 1)) {
      for (j in (i + 1):centersCount) {
          centersDists[i, j] <- getCentersDistance(centers[i], centers[j])
      }
    }
  }
  
  centersDists
}

getDmaxDmin <- function(centersDists) {
  centersCount <- nrow(centersDists)
  dmax <- -1
  dmin <- -1
  
  for (i in 1:(centersCount - 1)) {
    for (j in (i + 1):centersCount) {
      val <- centersDists[i, j]
      if (dmax == -1 | val > dmax) {
        dmax <- val
      }
      if (dmin == -1 | val < dmin) {
        dmin <- val
      }
    }
  }
  
  list(dmax = dmax, dmin = dmin)
}

getDis <- function(centersDists, dmax, dmin) {
  centersCount <- nrow(centersDists)
  outSum <- 0
  
  for (i in 1:(centersCount - 1)) {
    inSum <- 0
    for (j in (i + 1):centersCount) {
        inSum <- inSum + centersDists[i, j]
    }
    outSum <- outSum + (1 / inSum)
  }
  
  dis = (dmax / dmin) * outSum
    
  dis
}

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
  
  for(i in 1:datasetExamplesCount)
  {
    centerDist <- -1
    centersCount <- nrow(centers)
    
    for(j in 1:centersCount) {
      center <- centers[j,]
      centerDistTmp <- getDistance(data, datasetFirstAttrIdx, datasetAttrsCount, i, center)
      if (centerDist < 0 | centerDistTmp < centerDist) {
        centerDist <- centerDistTmp
      }
    }
    clusteringMetrics <- clusteringMetrics + centerDist
  }
  
  clusteringMetrics
}