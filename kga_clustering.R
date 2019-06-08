source("datasets.R")
source("k_means.R")
source("metrics.R")
source("ga.R")

# Chosen dataset to clustering process
currentDataset <- datasets()$IRIS
currentMaxCentersCount = 20

# Implemented genetic algorithm with defined parameters values
GA <- ga(dataset = currentDataset, palfa = 100.0,
         #maxCentersCount = currentMaxCentersCount,
         popSize = 100,
         maxIter = 1000,
         run = 50,
         pMutation = 0.5,
         pCrossover = 0.8,
         parallel = TRUE,
         cores = 5
)

# Results collection
population <- GA$population
bestSol <- GA$bestSol

# Reporting
print("Best found solution - centers:")
print(bestSol)
print(sprintf("Best found solution - clusters count: %d", nrow(bestSol)))
print(sprintf("Best found solution - clustering metrics: %f", getClusteringMetrics(currentDataset, bestSol)))
 
kMeansRes <- kMeans(dataset = currentDataset, centers = 3, maxIter = 1000)
print ("kMeans solution (with given clusters count) - centers:")
print (kMeansRes$centers)
print (sprintf("kMeans solution (with given clusters count) - clustering metrics: %f", kMeansRes$tot.withinss))