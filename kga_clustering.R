library(mlbench)

source("datasets.R")
source("k_means.R")
source("ga.R")


#kMeans(datasetEnum()$LETTER_RECOGNITION, NULL)

GA <- ga(popSize = 50, # population size
         maxiter = 1000, # max ga iterations number
         run = 50 # max number of generations without fitness function improvement
         )