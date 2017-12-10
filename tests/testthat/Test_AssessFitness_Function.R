#Test the AssessFitness function
context("Test the type of return value of AssessFitness function")

test_AssessFitness <- function(dataset, response.name){
  require(testthat)
  
  subsets <- ExtractResponseVariable(dataset, response.name)
  predictors <- subsets[[2]]
  set.seed(1)
  generation.old <- lapply(1, function(x) {rbinom(C, 1, 0.5)})
  
  test_that("Check type of return value of AssessFitness function",{
    set.seed(1)
    expect_equal(round(sapply(generation.old, AssessFitness, response = response, predictors = predictors)), 5536)
  })
}
