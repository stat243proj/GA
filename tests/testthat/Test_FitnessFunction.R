#Test the FitnessFunction
context("Test the output of FitnessFunction")

test_FitnessFunction <- function(dataset, response.name){
require(testthat)

subsets <- ExtractResponseVariable(dataset, response.name)
response <<- subsets[[1]]
model <- glm(response[,1]~., family="gaussian", data=predictors.individual)
userfunc <- "AIC"

#test FitnessFunction function
test_that("Check the return fitness value from FitnessFunction is 5435",{
  expect_equal(round(FitnessFunction(model, userfunc)), 5435)
})
}

