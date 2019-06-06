source("datasets.R")
source("k_means.R")
source("ga.R")
source("pop_init.R")
source("metrics.R")

currentDataset <- datasets()$IRIS
currentMaxCentersCount = 5
# individual <- initIndividual(dataset = currentDataset, centersCount = 5)
# data <- iris
# datasetAttrsCount = currentDataset$attrsCount
# datasetFirstAttrIdx = currentDataset$firstAttrIdx
# datasetLastAttrIdx = currentDataset$lastAttrIdx
# xmean <- vector("numeric", datasetAttrsCount)
# for (i in datasetFirstAttrIdx:datasetLastAttrIdx) {
#         xmean[i + 1 - datasetFirstAttrIdx] <- mean(data[, i])
# }
# xvar <- getVar(currentDataset, xmean)
# fitnessFun(currentDataset, individual, 10.0, xvar)
# print(kMeans(dataset = currentDataset, centers = individual, maxIter = 100))
# initPop(dataset = currentDataset, popSize = 5, maxCentersCount = 26)

GA <- ga(dataset = currentDataset, palfa = 10.0,
      #maxCentersCount = currentMaxCentersCount,
      popSize = 5,
      maxIter = 1000,
      run = 50,
      pMutation = 0.5,
      parallel = TRUE
   )

   population <- GA$population
   bestSol <- GA$bestSol

   print(sprintf("Best found solution - clusters count: %d", nrow(bestSol)))
   print("Best found solution - centers:")
   print(bestSol)
   
   print ("kMeans solution (with given clusters count):")
   print (kMeans(dataset = currentDataset, centers = 3, maxIter = 1000))