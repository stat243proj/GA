#Test the CrossOverMutate function
context("Test the dimension of the output of CrossOverMutate function")

#test CrossOverMutate function
 test_that("Check the value of return value(a child genome) from CrossOverMutate function is correct",{
    set.seed(1)
    generation <- lapply(1:40, function(x) {rbinom(27, 1, 0.5)})
    expect_equal(CrossOverMutate(generation, c(1,2), 0.004811252)[[1]], 
    c(0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1))
  })

test_that("Check the value of return value from CrossOverMutate function with a zero list is correct",{
  expect_equal(CrossOverMutate(lapply(1:40, function(x) {rep(0, times=27)}), 
                                 c(1,2), 0.004811252)[[1]], 
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  })
  
  test_that("Check the dimension of return value(a child genome) from CrossOverMutate function is correct",{
    set.seed(1)
    generation <- lapply(1:40, function(x) {rbinom(27, 1, 0.5)})
    expect_length(unlist(CrossOverMutate(generation, c(1,2), 0.004811252)), 54)
  })
