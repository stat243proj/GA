#Test the AssessFitness function

context("Test the return value of AssessFitness function")

test_that("Check the return value of AssessFitness function",{
  expect_equal(round(AssessFitness(rep(1, times=27),response, predictors, 
                               user.family = "gaussian", userfunc = "AIC")), 562)
  })
