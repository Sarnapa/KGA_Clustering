library(foreach)
library(doParallel)

source("fitness_fun.R")
source("centers_init.R")
source("selection.R")
source("mutation.R")
source("crossover.R")

ga <- function(popSize = 1000, 
               pcrossover = 0.8, 
               pmutation = 0.1, 
               elitism = base::max(1, round(popSize*0.05)),
               maxiter = 100,
               run = 10,
               parallel = TRUE,
               cores = 4) 
{
  
  if(parallel == TRUE){
    registerDoParallel(cores)
    
    foreach (i=1:3, .combine=c) %dopar% {
      sqrt(i)
    }
  }

  
}