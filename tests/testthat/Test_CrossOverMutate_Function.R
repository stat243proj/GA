#Test the CrossOverMutate function
context("Test the dimension of the output of CrossOverMutate function")

#test CrossOverMutate function
  test_that("Check the dimension of return value(a child genome) from Cross_over_mutate function is C",{
    set.seed(1)
    generation <- lapply(1:40, function(x) {rbinom(27, 1, 0.5)})
    expect_equal(length(unlist(CrossOverMutate(generation, c(1,2), 0.004811252))), 54)
  })
