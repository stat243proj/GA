#Test the Breed function
context("Test the dimension of the output of Breed function")

test_Breed <- function(dataset, response.name){
  require(testthat)
  
  C <- length(predictors)
  Niter <<- Niter
  P <<- as.integer(C*1.5)
  predictors <- dataset
  prob.mutate <- 1.0/(P*sqrt(C))
  generation.old <- lapply(1:P, function(x) {rbinom(C,1,0.5)})
  fitness <- matrix(0,P,Niter)
  
  test_that("Check the dimension of return list from Breed function is P embeded lists, 
            with length of each element(child genome) is C",{
              expect_equal(length(Breed(generation.old, fitness[,1], predictors, prob.mutate)), P/2)
              expect_equal(unique(unlist(
                lapply(Breed(generation.old, fitness[,1], predictors, prob.mutate),function(x) length(unlist(x))
                ))), 2*C)
})
}
