library(foreach)
library(doParallel)

source("fitness_fun.R")
source("pop_init.R")
source("selection.R")
source("mutation.R")
source("crossover.R")

ga <- function(dataset,
               maxCentersCount = -1,
               popSize = 100, # population size
               pCrossover = 0.8, # crossover probability
               pMutation = 0.1, # mutation probability
               maxIter = 100,  # max ga iterations number
               run = 100, # max number of generations without fitness function improvement
               parallel = TRUE,
               cores = 4) {
  
  # dataset settings
  data <- NULL
  datasetId = dataset$id
  datasetAttrsCount = dataset$attrsCount
  datasetFirstAttrIdx = dataset$firstAttrIdx
  datasetLastAttrIdx = dataset$lastAttrIdx
  
  if (datasetId == datasets()$IRIS$id) {
    if(!exists("iris"))
      data(iris)
    data <- iris
  }
  else if (datasetId == datasets()$LETTER_RECOGNITION$id) {
    if(!exists("LetterRecognition"))
      data(LetterRecognition)
    data <- LetterRecognition
  }
  
  # min max attr vectors
  xmin <- vector("numeric", datasetAttrsCount)
  xmax <- vector("numeric", datasetAttrsCount)
  
  for(i in datasetFirstAttrIdx:datasetLastAttrIdx) {
    xmin[i+1-datasetFirstAttrIdx] <- min(data[, i])
    xmax[i+1-datasetFirstAttrIdx] <- max(data[, i])
  }
  
  # parallel settings
  if(parallel == TRUE) {
    registerDoParallel(cores)
  }
  
  population <- initPop(dataset, popSize, maxCentersCount)
  maxPopSize <- popSize
  runs <- 0
  bestSol <- 0
  bestFitVal <- 0

  for(i in seq_len(maxIter)) {
    
    if(parallel)
    {
      stime <- system.time( {
  
        fitness <- foreach (j=0:popSize, .combine=c) %dopar% {
          fitnessFun(dataset, population[j])
        }
      })
      
      print(stime[3])
    } else {
      fitness <- foreach (j=0:popSize, .combine=c) %do% {
        fitnessFun(dataset, population[j])
      }
    }
    print(fitness)
    
    # check end conditions
    newBestFitVal <- max(fitness)
    if(newBestFitVal > bestFitVal) {
      bestFitVal <- newBestFitVal
      bestSol <- population[which.max(fitness)]
      runs <- 0
    } else {
      runs <- runs + 1
    }
    
    if(runs > run) {
      break
    }
    
    # selection
    Selection <- selection(population, fitness, maxPopSize)
    newPopulation <- Selection[1]
    newFitness <- Selection[2]
    newPopSize <- length(newPopulation)
    
    # crossover
    nmating <- floor(newPopSize/2)
    # mating matrix
    mating <- matrix(sample(1:(2*nmating), size = (2*nmating)), ncol = 2)
    for(i in seq_len(nmating)) {
      # crossover with given probability pcrossover
      if(pcrossover > runif(1)) {
        # choosing parents
        parents <- c(newPopulation[mating[i,1]], newPopulation[mating[i,2]])
        Crossover <- crossover(parents)
        # adding children to new population
        newPopulation.append(Crossover[1])
        newPopulation.append(Crossover[2])
      }
    }
    
    
    Mmin <- 1 / max(fitness)
    Mmax <- 1 / min(fitness)
    
    # update new population size after operations
    # newPopSize <- length(newPopulation)
    
    # mutation (only on parents)
    for(i in seq_len(newPopSize)) {
      # mutation with given probability pcrossover
      if(pmutation > runif(1)) {
        # mutating
        M = 1 / newFitness[i]
        newPopulation[i] <- mutation(newPopulation[i], M, Mmin, Mmax, xmin, xmax)
      }
    }
    
    # update population
    population <- newPopulation
    popSize <- length(newPopulation)
  }
}