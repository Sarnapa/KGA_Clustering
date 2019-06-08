library(foreach)
library(doParallel)

source("fitness_fun.R")
source("pop_init.R")
source("selection.R")
source("mutation.R")
source("crossover.R")
source("metrics.R")

# Definiton of genetic algorithm
ga <- function(dataset, # given dataset info
               maxCentersCount = -1, # max number of clusters / centers
               palfa = 1.0, # rate associated with g factor
               popSize = 100, # population size
               pCrossover = 0.8, # crossover probability
               pMutation = 0.1, # mutation probability
               maxIter = 100,  # max ga iterations number
               run = 100, # max number of generations without fitness function improvement
               parallel = TRUE,
               cores = 4) {
  
  # Dataset settings
  data <- NULL
  datasetId = dataset$id
  datasetAttrsCount = dataset$attrsCount
  datasetFirstAttrIdx = dataset$firstAttrIdx
  datasetLastAttrIdx = dataset$lastAttrIdx
  datasetExamplesCount = dataset$examplesCount
  datasetIsInteger = dataset$isInteger
  
  if (maxCentersCount == -1) {
    maxCentersCount = datasetExamplesCount
  }
  
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
  
  # Min max attr vectors
  xmin <- vector("numeric", datasetAttrsCount)
  xmean <- vector("numeric", datasetAttrsCount)
  xmax <- vector("numeric", datasetAttrsCount)
  
  # Get data variance
  xvar <- getVar(dataset, xmean)
  
  # Data stats preparation
  for(i in datasetFirstAttrIdx:datasetLastAttrIdx) {
    xmin[i+1-datasetFirstAttrIdx] <- min(data[, i])
    xmean[i+1-datasetFirstAttrIdx] <- mean(data[, i])
    xmax[i+1-datasetFirstAttrIdx] <- max(data[, i])
  }
  
  # Parallel settings
  if(parallel == TRUE) {
    registerDoParallel(cores)
  }
  
  # Population initialization
  population <- initPop(dataset, popSize, maxCentersCount)
  maxPopSize <- popSize
  runs <- 0
  bestSol <- 0
  bestFitVal <- 0

  # Evaluation process
  for(i in seq_len(maxIter)) {
    print(sprintf("Iteration: %d", i))
    if(parallel)
    {
      stime <- system.time( {
        fitness <- foreach (j=1:popSize, .combine=c) %dopar% {
          source("fitness_fun.R")
          fitnessFun(dataset, population[[j]], palfa, xvar)
        }
      })
      
      print(sprintf("Fitness function execution time: %f", stime[3]))
    } else {
        fitness <- foreach (j=1:popSize, .combine=c) %do% {
        source("fitness_fun.R")
        fitnessFun(dataset, population[[j]], palfa, xvar)
      }
    }
    
    # Check end conditions
    newBestFitVal <- max(fitness)
    if(newBestFitVal > bestFitVal) {
      bestFitVal <- newBestFitVal
      bestSol <- population[[which.max(fitness)]]
      print(sprintf("New best solution - clusters count: %d", nrow(bestSol)))
      runs <- 0
    } else {
      runs <- runs + 1
    }
    
    if(runs > run) {
      print("Still without improvements so it's high time to go :)")
      break
    }

    # Selection process
    print("Selection operator processing...")
    selectionRes <- selection(population, fitness, maxPopSize)
    population <- selectionRes$population
    orderedFitness <- selectionRes$fitness

    print("Current fitness function values:")
    print(orderedFitness)
    
    newPopulation <- list()

    # Crossover process
    print("Crossover operator processing...")
    nmating <- floor(length(population)/2)
    # Mating matrix
    mating <- matrix(sample(1:(2*nmating), size = (2*nmating)), ncol = 2)
    for(i in seq_len(nmating)) {
      # Crossover with given probability pcrossover
      if(pCrossover > runif(1)) {
        # Choosing parents
        parents <- c(population[mating[i, 1]], population[mating[i, 2]])
        Crossover <- crossover(parents)
        # Adding children to new population
        newPopulationSize <- length(newPopulation)
        newPopulation[[newPopulationSize + 1]] <- Crossover[[1]]
        newPopulation[[newPopulationSize + 2]] <- Crossover[[2]]
      }
    }
    
    # Update new population size after operations
    newPopSize <- length(newPopulation)

    # Mutation process (only on parents)
    print("Mutation operator processing...")
    for(i in seq_len(newPopSize)) {
      # Mutation with given probability pcrossover
      if(pMutation > runif(1, 0, 1)) {
        newPopulation[[i]] <- mutation(newPopulation[[i]], xmin, xmax, 
                                       maxCentersCount, datasetIsInteger)
      }
    }
    
    print("Created new population")
    # Update population
    population <- c(population, newPopulation)
    popSize <- length(population)
  }
  
  list(population = population, bestSol = bestSol)
}