#Test the CrossOverMutate function
context("Test the dimension of the output of CrossOverMutate function")

test_CrossOverMutate <- function(dataset, response.name){
  require(testthat)
  
  subsets <- ExtractResponseVariable(dataset, response.name)
  predictors <- subsets[[2]]
  C <- length(predictors)
  Niter <<- Niter
  P <- as.integer(C*1.5)
  P <<- 2*ceiling(P/2)
  prob.mutate <- 1.0/(P*sqrt(C))
  generation.old <- lapply(1:P, function(x) {rbinom(C,1,0.5)})
  
  #test CrossOverMutate function
  test_that("Check the dimension of return value(a child genome) from Cross_over_mutate function is C",{
    expect_equal(length(unlist(CrossOverMutate(generation.old, c(1,2), prob.mutate))), 2*C)
  })
}
