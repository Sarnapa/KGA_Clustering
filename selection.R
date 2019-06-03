selection <- function(population, fitness, n){
  tmp <- matrix(cbind(fintess, population), ncol = 2)
  tmp <- tmp[order(tmp[,1], decreasing=TRUE),]
  tmp <- tmp[1:n,]
}