library(mlbench)

source("datasets.R")
source("k_means.R")
source("ga.R")
source("centers_init.R")

#currentDataset <- datasets()$IRIS
#centers <- initCenters(currentDataset, 5)
#kMeans(currentDataset, centers)

GA <- ga(popSize = 50,
         maxiter = 1000,
         run = 50
)