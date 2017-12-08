library(testthat)
library(GA)

test_check("GA")

predictors <- dataset
C <- length(predictors)
Niter <<- Niter
P <<- as.integer(C*1.5)
prob.mutate <- 1.0/(P*sqrt(C))
generation.old <- lapply(1:P, function(x) {rbinom(C,1,0.5)})
fitness <- matrix(0,P,Niter)
model.out <- lm(response[,1]~., predictors.individual)
fitness.value <- extractAIC(model.out)[2]

#test the Breed function
test_that("Check the dimension of return list from Breed function is P embeded lists, 
          with length of each element(child genome) is C",{
            expect_equal(length(Breed(generation.old, fitness[,1], predictors, prob.mutate)), P/2)
            expect_equal(unique(unlist(
              lapply(Breed(generation.old, fitness[,1], predictors, prob.mutate),function(x) length(unlist(x))
              ))), 2*C)
            })

#test CrossOverMutate function
test_that("Check the dimension of return value(a child genome) from Cross_over_mutate function is C",{
  expect_equal(length(unlist(CrossOverMutate(generation.old, c(1,2), prob.mutate))), 2*C)
})

#test ReplaceClones function
test_that("Check the dimension of return generation from ReplaceClones function is P",{
  expect_equal(length(ReplaceClones(generation.old, fitness, C)$generation), P)
})