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
               run = maxIter, # max number of generations without fitness function improvement
               parallel = TRUE,
               cores = 4) {
  
  # passing dataset not working
  
  # TMPvec
  # dataset <- datasets()$IRIS
  # 
  # # dataset settings
  # datasetId = dataset$id
  # datasetAttrsCount = dataset$attrsCount
  # datasetFirstAttrIdx = dataset$firstAttrIdx
  # datasetLastAttrIdx = dataset$lastAttrIdx
  # 
  # if (datasetId == datasets()$IRIS$id) {
  #   if(!exists("iris"))
  #     data(iris)
  # }
  # else if (datasetId == datasets()$LETTER_RECOGNITION$id) {
  #   if(!exists("LetterRecognition"))
  #     data(LetterRecognition)
  # }
  
  # for testing
  # dataset = iris
  # 
  # datasetAttrsCount = 4
  # datasetFirstAttrIdx = 1
  # datasetLastAttrIdx = 4
  # 
  # library(mlbench)
  # data(LetterRecognition)
  # dataset = LetterRecognition
  # 
  # datasetAttrsCount = 16
  # datasetFirstAttrIdx = 2
  # datasetLastAttrIdx = 17
  
  # min max attr vectors
  xmin <- vector("numeric", datasetAttrsCount)
  xmax <- vector("numeric", datasetAttrsCount)
  
  for(i in datasetFirstAttrIdx:datasetLastAttrIdx) {
    xmin[i+1-datasetFirstAttrIdx] <- min(dataset[i])
    xmax[i+1-datasetFirstAttrIdx] <- max(dataset[i])
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
          fitnessFun(population[j])
        }
      })
      
      print(stime[3])
    } else {
      fitness <- foreach (j=0:popSize, .combine=c) %do% {
        fitnessFun(population[j])
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
    newPopulation <- selection(population, fitness, maxPopSize)
    newPopSize <- length(newPopulation)
    
    # crossover
    nmating <- floor(newPopSize/2)
    # mating matrix
    mating <- matrix(sample(1:(2*nmating), size = (2*nmating)), ncol = 2)
    for(i in seq_len(nmating)) {
      # crossover with given probability pcrossover
      if(pcrossover > runif(1)) {
        # choosing parents
        parents <- c(population[mating[i,1]], population[mating[i,2]])
        Crossover <- crossover(parents)
        # adding children to new population
        newPopulation.append(Crossover[1])
        newPopulation.append(Crossover[2])
      }
    }
    
    
    Mmin <- 1 / max(fitness)
    Mmax <- 1 / min(fitness)
    
    # update new population size after crossover
    newPopSize <- length(newPopulation)
    # mutation
    for(i in seq_len(newPopSize)) {
      # mutation with given probability pcrossover
      if(pmutation > runif(1)) {
        # mutating
        newPopulation[i] <- mutation(newPopulation[i], Mmin, Mmax, xmin, xmax)
      }
    }
    
    # update population
    population <- newPopulation
    popSize <- length(newPopulation)
  }
}