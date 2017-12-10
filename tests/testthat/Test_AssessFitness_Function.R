#Test the AssessFitness function

context("Test the return value of AssessFitness function")

test_that("Check the return value of AssessFitness function",{
  set.seed(1)
  individual <- rbinom(27, 1, prob=0.5)
  expect_equal(round(AssessFitness(individual,response, predictors, 
                               user.family = "gaussian", userfunc = "AIC")), 852)
  })
