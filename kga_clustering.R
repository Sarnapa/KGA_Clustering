source("datasets.R")
source("k_means.R")
source("ga.R")
source("pop_init.R")
source("fitness_fun.R")

currentDataset <- datasets()$IRIS
currentMaxCentersCount = 150
#individual <- initIndividual(dataset = currentDataset, centersCount = 3)
#rating <- fitnessFun(dataset = currentDataset, individual)
#kMeans(dataset = currentDataset, centers = individual, maxIter = 1)
#initPop(dataset = currentDataset, popSize = 5, maxCentersCount = 26)

GA <- ga(dataset = currentDataset,
         #maxCentersCount = currentMaxCentersCount,
         popSize = 2,
         maxIter = 1000,
         run = 50,
         parallel = FALSE
)