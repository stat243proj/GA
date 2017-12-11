#Test the Select function
context("Test the type of return value of Selection function")

  #test Select function
 test_that("Check the output of the Select function is correct",{
  set.seed(7)
  output <- Select(baseball.dat, "salary", userfunc="AIC", user.family="gaussian", flag.log.scale=TRUE,
                   frac.replace=0.2, Niter=100, mutate.rate=FALSE, plot.flag=TRUE)
   expect_equal(sum(output$bestModel$residuals), 1.062084e-15)
   expect_equal(output$LastGen[[1]], 
                 c(1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0))
  })
