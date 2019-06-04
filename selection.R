selection <- function(population, fitness, n){
  order <- order(fitness, decreasing = TRUE)
  fitness <- fitness[order]
  population <- population[order]
  fitness <- fitness[1:n]
  population <- population[1:n]
  list(newPopulation = population, fitness = fitness)
}