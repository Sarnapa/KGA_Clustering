# Selection process
selection <- function(population, fitness, n){
  order <- order(fitness, decreasing = TRUE)
  fitness <- fitness[order]
  population <- population[order]
  fitness <- fitness[1:n]
  population <- population[1:n]
  list(population = population, fitness = fitness)
}