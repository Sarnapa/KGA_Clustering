library(mlbench)
library(GA)

source("datasets.R")
source("k_means.R")

source("fitness_fun.R")
source("centers_init.R")
source("selection.R")
source("mutation.R")
source("crossover.R")

#kMeans(datasetEnum()$LETTER_RECOGNITION, NULL)

GA <- ga(type = "real-valued", 
         fitness = fitness_fun,
         population = centers_init,
         selection = selection,
         crossover = crossover,
         mutation = mutation,
         #lower= , upper=  # do sprawdzenia, pisze ze wymagane, a w docu ze deprecated
         popSize = 50, maxiter = 1000, run = 100)

plot(GA)
summary(GA)