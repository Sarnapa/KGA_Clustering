source("datasets.R")
source("k_means.R")
source("ga.R")
source("pop_init.R")
source("fitness_fun.R")

currentDataset <- datasets()$IRIS
currentMaxCentersCount = 15
#individual <- initIndividual(dataset = currentDataset, centersCount = 1)
#print(rating <- fitnessFun(dataset = currentDataset, individual))
#print(kMeans(dataset = currentDataset, centers = individual, maxIter = 100))
#initPop(dataset = currentDataset, popSize = 5, maxCentersCount = 26)

GA <- ga(dataset = currentDataset,
   maxCentersCount = currentMaxCentersCount,
   popSize = 2,
   maxIter = 1000,
   run = 50,
   pMutation = 0.5,
   parallel = TRUE
)

population <- GA$population
bestSol <- GA$bestSol

print(sprintf("Best solution - clusters count: %d", nrow(bestSol)))
print("Best solution - centers:")
print(bestSol)