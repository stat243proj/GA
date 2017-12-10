#Test the ReplaceClones function
context("Test the dimension and value of the output of ReplaceClones function")

test_that("Check the output of the ReplaceClones function is correct",{
    
    expect_equal(ReplaceClones(lapply(1:40, function(x) {rep(1, times=27)}), 
                                     matrix(0,40,100), 27)$generation[[1]], 
                 c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
    
    expect_equal(ReplaceClones(lapply(1:40, function(x) {rep(NA, times=27)}), 
                                     matrix(0,40,100), 27)$generation[[1]], 
                 c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
    
  })

  test_that("check the dimension of return generation from ReplaceClones function is correct",{
    expect_length(ReplaceClones(lapply(1:40, function(x) {rep(1, times=27)}),
                                matrix(0,40,100), 27)$generation, 40)
  })

