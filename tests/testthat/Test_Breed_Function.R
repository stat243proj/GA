#Test the Breed function
context("Test the dimension and value of the output of Breed function")

test_that("With the baseball data, check the output of the Breed function is correct",{
              set.seed(1)
              generation <- lapply(1:40, function(x) {rbinom(27, 1, 0.5)})
              set.seed(1)
              fitness.vec <- rnorm(40, 700, 1)
              expect_equal(Breed(generation, fitness.vec, 0.004811252)[[1]], 
               c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1))
})

test_that("Check the output of the Breed function with NA input is correct",{
expect_equal(Breed(lapply(1:40, function(x) {rep(NA, times=27)}),
                                 fitness.vec, 0.004811252)[[1]],
                           as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)))
})

test_that("With the baseball data, check the dimension and length of return list from Breed function are correct",{
              set.seed(1)
              generation <- lapply(1:40, function(x) {rbinom(C, 1, 0.5)})
              set.seed(1)
              fitness.vec <- rnorm(40, 700, 1)
              expect_length(Breed(generation, fitness.vec, 0.004811252), 40)
              expect_equal(unique(unlist(
                lapply(Breed(generation, fitness.vec, 0.004811252),function(x) length(unlist(x))
                ))), 27)
})
