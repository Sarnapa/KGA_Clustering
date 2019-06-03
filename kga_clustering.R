library(mlbench)
library(GA)

source("datasets.R")
source("fitness_fun.R")
source("k_means.R")

#kMeans(datasetEnum()$LETTER_RECOGNITION, NULL)

GA <- ga(type = "real-valued", 
         fitness = fitness_function, 
         popSize = 50, maxiter = 1000, run = 100)

plot(GA)
summary(GA)