#Test the Breed function
context("Test the dimension and value of the output of Breed function")

test_that("With the baseball data, check the dimension of return list from Breed function is 39 embeded lists, 
            with length of each element(child genome) is 27",{
              set.seed(1)
              generation <- lapply(1:40, function(x) {rbinom(C, 1, 0.5)})
              set.seed(1)
              fitness.vec <- rnorm(40, 700, 1)
              expect_equal(length(Breed(generation, fitness.vec, 0.004811252)), 39)
              expect_equal(unique(unlist(
                lapply(Breed(generation, fitness.vec, 0.004811252),function(x) length(unlist(x))
                ))), 27)
              expect_equal(Breed(generation, fitness.vec, 0.004811252)[[1]], 
               c(0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0))
})
