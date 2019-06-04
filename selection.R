selection <- function(population, fitness, n){
  order <- order(fitness)
  fitness <- fitness[order]
  population[order]
  fitness <- fitness[1:n]
  population <- population[1:n]
  list(newPopulation = population, fitness = fitness)
}