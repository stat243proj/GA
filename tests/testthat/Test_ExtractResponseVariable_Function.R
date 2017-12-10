#Test the ExtractResponseVariable
context("Test the output of ExtractResponseVariable")

test_ExtractResponseVariable <- function(dataset, response.name){
  require(testthat)
  
  #test ExtractResponseVariable function
  test_that("Check the length of baseball.dat response variables is 337",{
    expect_equal(length(unlist(ExtractResponseVariable(baseball.dat, "salary")[[1]])), 337)
  })
}