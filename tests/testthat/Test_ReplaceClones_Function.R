#Test the ReplaceClones function
context("Test the dimension of the output of ReplaceClones function")

test_ReplaceClones <- function(dataset, response.name){
  require(testthat)
  
  C <- length(predictors)
  Niter <<- Niter
  P <<- as.integer(C*1.5)
  predictors <- dataset
  prob.mutate <- 1.0/(P*sqrt(C))
  generation.old <- lapply(1:P, function(x) {rbinom(C,1,0.5)})
  fitness <- matrix(0,P,Niter)
  
  #test ReplaceClones function
  test_that("Check the dimension of return generation from ReplaceClones function is P",{
    expect_equal(length(ReplaceClones(generation.old, fitness, C)$generation), P)
  })
}

