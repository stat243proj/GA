#Test the Breed function
context("Test the dimension of the output of Breed function")

test_Breed <- function(dataset, response.name){
  require(testthat)
  
  subsets <- ExtractResponseVariable(dataset, response.name)
  predictors <- subsets[[2]]
  C <- length(predictors)
  Niter <<- Niter
  P <- as.integer(C*1.5)
  P <<- 2*ceiling(P/2)
  prob.mutate <- 1.0/(P*sqrt(C))
  generation.old <- lapply(1:P, function(x) {rbinom(C,1,0.5)})
  fitness <- matrix(0,P,Niter)
  set.seed(1)
  
  test_that("Check the dimension of return list from Breed function is P embeded lists, 
            with length of each element(child genome) is C",{
              expect_equal(length(Breed(generation.old, fitness[,1], prob.mutate)), P-1)
              expect_equal(unique(unlist(
                lapply(Breed(generation.old, fitness[,1], prob.mutate),function(x) length(unlist(x))
                ))), C)
              set.seed(1)
              expect_equal(Breed(generation.old, fitness[,1], prob.mutate)[[1]], 
               c(0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0))
})
}
