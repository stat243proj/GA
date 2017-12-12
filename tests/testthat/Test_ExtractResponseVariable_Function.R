#Test the ExtractResponseVariable function
data(mtcars)
C <- dim(mtcars)[2] - 1
P <- as.integer(C*1.5)
P <- 2*ceiling(P/2)
prob.mutate <- 1.0/(P*sqrt(C))
context("Test the output of ExtractResponseVariable")
test_that("Check the length of dataset mtcars response variables is 32,
            and the first and third elements of salary are 21.0 and 22.8",{
  data(mtcars)
  expect_equal(length(unlist(ExtractResponseVariable(mtcars, "mpg")[[1]])), 32)
  expect_equal(as.numeric((unlist(ExtractResponseVariable(mtcars, "mpg")[[1]]))[1]), 21.0)
  expect_equal(as.numeric((unlist(ExtractResponseVariable(mtcars, "mpg")[[1]]))[3]), 22.8)
})
