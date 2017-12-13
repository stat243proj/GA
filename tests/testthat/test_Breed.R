#Test the Breed function
data(mtcars)
C <- dim(mtcars)[2] - 1
P <- as.integer(C*1.5)
P <- 2*ceiling(P/2)
prob.mutate <- 1.0/(P*sqrt(C))
context("Test the dimension and value of the output of Breed function")
test_that("With the baseball data, check the output of the Breed function is correct",{
  set.seed(1)
  generation <- lapply(1:P, function(x) {rbinom(C, 1, 0.5)})
  fitness.vec <- rnorm(P, 160, 10)
  expect_equal(Breed(generation, fitness.vec, prob.mutate)[[1]],
               c(0, 0, 0, 1, 1, 1, 1, 1, 0, 0))
})
