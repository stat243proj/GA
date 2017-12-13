#Test the CrossOverMutate function
data(mtcars)
C <- dim(mtcars)[2] - 1
P <- as.integer(C*1.5)
P <- 2*ceiling(P/2)
prob.mutate <- 1.0/(P*sqrt(C))
context("Test the dimension of the output of CrossOverMutate function")
test_that("Check the value of return value(a child genome) from CrossOverMutate function is correct",{
  set.seed(2)
  generation <- lapply(1:P, function(x) {rbinom(C, 1, 0.5)})
  expect_equal(CrossOverMutate(generation, c(1,2), prob.mutate)[[1]],
               c(0, 1, 1, 1, 1, 0, 0, 1, 0, 1))
  expect_equal(CrossOverMutate(generation, c(3,4), prob.mutate)[[1]],
               c(1, 1, 1, 1, 0, 0, 1, 0, 0, 1))
  expect_length(unlist(CrossOverMutate(generation, c(5,6), prob.mutate)), 20)
})
