#Test the AssessFitness function

context("Test the return value of AssessFitness function")

test_that("Check the return value of AssessFitness function with baseball data",{
  expect_equal(round(AssessFitness(rep(1, times=27),baseball.dat["salary"], baseball.dat, 
                               user.family = "gaussian", userfunc = "AIC")), -18653)
  })

test_that("Check the return value of AssessFitness function with mtcars data",{
  data(mtcars)
  expect_equal(round(AssessFitness(rep(1, times=10), mtcars["mpg"], mtcars, 
                               user.family = "gaussian", userfunc = "AIC")), -2036)
  })
