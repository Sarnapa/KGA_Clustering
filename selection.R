selection <- function(population, fitness, n){
  tmp <- matrix(cbind(fitness, population), ncol = 2)
  tmp <- tmp[order(tmp[,1], decreasing=TRUE),]
  tmp <- tmp[1:n,]
  pop <- tmp[,2]
  fit <- tmp[,1]
  list(pop, fit)
}