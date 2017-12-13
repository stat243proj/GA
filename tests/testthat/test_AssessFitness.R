#Test AssessFitness function
data(mtcars)
C <- dim(mtcars)[2] - 1
P <- as.integer(C*1.5)
P <- 2*ceiling(P/2)
prob.mutate <- 1.0/(P*sqrt(C))
context("Test the return value of AssessFitness function")
test_that("Check the return value of AssessFitness function with mtcars data",{
  individual <- c(0,1,0,1,0,0,1,0,1,1)
  expect_equal(floor(AssessFitness(individual, mtcars["mpg"], mtcars,
                user.family = "gaussian", userfunc = "AIC")), 156)
})
