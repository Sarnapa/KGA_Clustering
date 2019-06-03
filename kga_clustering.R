library(mlbench)

source("datasets.R")
source("k_means.R")
source("ga.R")


#kMeans(datasetEnum()$LETTER_RECOGNITION, NULL)

GA <- ga(popSize = 50,
         maxiter = 1000,
         run = 50 
         )