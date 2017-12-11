#Test the Select function
context("Test the type of return value of Selection function")

#test Select function
#test Select function with baseball data
 test_that("Check the output of the Select function with baseball data is correct",{
  set.seed(7)
  output <- Select(baseball.dat, "salary", userfunc="AIC", user.family="gaussian", flag.log.scale=TRUE,
                   frac.replace=0.2, Niter=100, mutate.rate=FALSE, plot.flag=TRUE)
   expect_equal(sum(output$bestModel$residuals), 1.062084e-15)
   expect_equal(output$LastGen[[1]], 
                 c(1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0))
  })

test_that("Check the dimension of fitness matrix of the Select function with baseball data", {
  set.seed(7)
  output <- Select(baseball.dat, "salary", userfunc="AIC", user.family="gaussian", flag.log.scale=TRUE,
                   frac.replace=0.2, Niter=100, mutate.rate=FALSE, plot.flag=TRUE)
  expect_equal(dim(output$fitness)[1], 40)
  expect_equal(dim(output$fitness)[2], 100)
})

#test Select function with mtcars data
test_that("Check the output of the Select function with mtcars data is correct",{
  set.seed(8)
  result <- Select(dataset=cars, response.name="mpg", user.family="gaussian", flag.log.scale=TRUE, Niter = 50, frac.replace = 0.2, mutate.rate = FALSE)
    expect_equal(sum(result$bestModel$residuals), 6.938894e-18)
    expect_equal(result$LastGen[[1]], 
                 c(1, 0, 1, 0, 1, 0, 0, 0, 0, 0))
  })

test_that("Check the dimension of fitness matrix of the Select function with mtcars data", {
  set.seed(8)
  result <- Select(dataset=cars, response.name="mpg", user.family="gaussian", flag.log.scale=TRUE, Niter = 50, frac.replace = 0.2, mutate.rate = FALSE)
  expect_equal(dim(result$fitness)[1], 16)
  expect_equal(dim(result$fitness)[2], 50)
})
