library(mlbench)
library(GA)

source("prep_data.R")
source("fitness_fun.R")

data(LetterRecognition)
data(iris)

print(getData(iris))
print(getData(LetterRecognition))

GA <- ga(type = "real-valued", 
         fitness = fitness_function, 
         popSize = 50, maxiter = 1000, run = 100)

plot(GA)
summary(GA)