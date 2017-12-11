#Test the ReplaceClones function
context("Test the dimension and value of the output of ReplaceClones function")

test_that("Check the output of the ReplaceClones function is correct",{
    set.seed(1)
    fitness.vec <- rnorm(40, 700, 1)
    expect_equal(ReplaceClones(lapply(1:40, function(x) {rep(1, times=27)}), 
                                     fitness.vec)$generation[[1]], 
                 c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
    
    expect_equal(ReplaceClones(lapply(1:40, function(x) {rep(NA, times=27)}), 
                                     fitness.vec)$generation[[1]], 
                 c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
    
  })

  test_that("check the dimension of return generation from ReplaceClones function is correct",{
    expect_length(ReplaceClones(lapply(1:40, function(x) {rep(1, times=27)}),
                                fitness.vec)$generation, 40)
  })

test_that("Check the output of the ReplaceClones function with mtcars data is correct",{
    set.seed(2)
  fitness.vec <- rnorm(16, 500, 20)
    expect_equal(ReplaceClones(lapply(1:16, function(x) {rep(1, times=10)}), 
                                     fitness.vec)$generation[[1]], 
                 c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
    
    expect_equal(ReplaceClones(lapply(1:16, function(x) {rep(NA, times=10)}), 
                                     fitness.vec)$generation[[1]], 
                 c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
    
  })

test_that("check the dimension of return generation from ReplaceClones function is correct",{
    expect_length(ReplaceClones(lapply(1:16, function(x) {rep(1, times=10)}),
                                fitness.vec)$generation, 16)
  })


