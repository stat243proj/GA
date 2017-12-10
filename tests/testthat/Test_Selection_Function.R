#Test the Selection function
context("Test the type of return value of Selection function")

test_Selection <- function(dataset, response.name){
  require(testthat)
  
  #test Selection function
  test_that("Check selection function",{
    expect_equal(Selection(baseball.dat, "salary", userfunc="AIC", user.family="gaussian", flag.log.scale=TRUE,
                           frac.replace=0.2, Niter=100, mutate.rate=FALSE, plot.flag=TRUE), )
  })
}