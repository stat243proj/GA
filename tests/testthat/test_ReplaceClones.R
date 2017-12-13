#Test the ReplaceClones function
data(mtcars)
C <- dim(mtcars)[2] - 1
P <- as.integer(C*1.5)
P <- 2*ceiling(P/2)
prob.mutate <- 1.0/(P*sqrt(C))
context("Test the dimension and value of the output of ReplaceClones function")
test_that("Check the output of the ReplaceClones function is correct",{
  set.seed(1)
  subsets <- ExtractResponseVariable(mtcars, "mpg")
  predictors <<- subsets[[2]]
  response <<- subsets[[1]]
  generation <- lapply(1:P, function(x) {rbinom(C, 1, 0.5)})
  fitness.vec <- rep(0, 16)
  pos <- sample(1:P, 5, prob = rep(1/16, 16))
  fitness.vec <- rnorm(P, 550, 20)
  fitness.vec[pos] <- 500
  expect_equal(ReplaceClones(generation, fitness.vec)$generation[[1]],
               c(0, 0, 1, 1, 0, 1, 1, 1, 1, 0))
})



