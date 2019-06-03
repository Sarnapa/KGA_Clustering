library(foreach)
library(doParallel)

source("fitness_fun.R")
source("centers_init.R")
source("selection.R")
source("mutation.R")
source("crossover.R")

ga <- function(popSize = 1000, # population size
               pCrossover = 0.8, # crossover probability
               pMutation = 0.1, # mutation probability
               maxIter = 100,  # max ga iterations number
               run = maxIter, # max number of generations without fitness function improvement
               parallel = TRUE,
               cores = 4) 
{
  
  if(parallel == TRUE){
    registerDoParallel(cores)
    
    # testing parallel efficiency
    # x <- iris[which(iris[,5] != "setosa"), c(1,5)]
    # trials <- 10000
    # 
    # 
    # stime <- system.time({
    #   res <- foreach(icount(trials), .combine=cbind) %do% {
    #     ind <- sample(100, 100, replace=TRUE)
    #     result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
    #     coefficients(result1)
    #   }
    # })
    
    #print(stime[3])
    #print(res)
    
  }
  
  population <- centers_init(popSize)
  runs <- 0
  bestSol <- 0
  bestFitVal <- 0

  for(i in seq_len(maxIter)){

    stime <- system.time({

      fitness <- foreach (j=0:popSize, .combine=c) %dopar% {
        fitness_fun(population[j])
      }

    })
    
    print(stime[3])
    print(fitness)
    
    # check end conditions
    newBestFitVal <- max(fitness)
    if(newBestFitVal > bestFitVal){
      bestFitVal <- newBestFitVal
      bestSol <- population[which.max(fitness)]
      runs <- 0
    } else {
      runs <- runs + 1
    }
    
    if(runs > run){
      break
    }
    
    # selection
    newPopulation <- selection(population, fitness)
    newPopSize <- length(newPopulation)
    
    # crossover
    nmating <- floor(newPopSize/2)
    # mating matrix
    mating <- matrix(sample(1:(2*nmating), size = (2*nmating)), ncol = 2)
    for(i in seq_len(nmating)){
      # crossover with given probability pcrossover
      if(pcrossover > runif(1)){
        # choosing parents
        parents <- c(population[mating[i,1]], population[mating[i,2]])
        Crossover <- crossover(parents)
        # adding child to new population
        newPopulation.append(Crossover)
      }
    }
    
    # update new population size after crossover
    newPopSize <- length(newPopulation)
    # mutation
    for(i in seq_len(newPopSize)){
      # mutation with given probability pcrossover
      if(pmutation > runif(1)){
        # mutating
        newPopulation[i] <- mutation(newPopulation[i])
      }
    }
    
    # update population
    population <- newPopulation
    popSize <- length(newPopulation)
  }
}