source("datasets.R")
source("k_means.R")
source("ga.R")
source("pop_init.R")

currentDataset <- datasets()$LETTER_RECOGNITION
currentMaxCentersCount = 150
#individual <- initIndividual(dataset = currentDataset, centersCount = 5)
#kMeans(dataset = currentDataset, centers = individual, maxIter = 1)
#initPop(dataset = currentDataset, popSize = 5, maxCentersCount = 26)

GA <- ga(dataset = currentDataset,
         #maxCentersCount = currentMaxCentersCount,
         popSize = 50,
         maxiter = 1000,
         run = 50,
)