#Test the AssessFitness function
context("Test the type of return value of AssessFitness function")

test_AssessFitness <- function(dataset, response.name){
  require(testthat)
  
  subsets <- ExtractResponseVariable(dataset, response.name)
  predictors <- subsets[[2]]
  
  test_that("Check type of return value of AssessFitness function",{
    expect_equal(class(
      AssessFitness(individual=rep(1, dim(baseball.dat)[2]), response="salary", 
                    predictors, user.family="gaussian", userfunc="AIC")
    ), "numeric")
  })
}