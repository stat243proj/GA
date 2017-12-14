#Test the Select function
context("Test the type of return value of Selection function")
set.seed(7)
#test Select function
#test Select function with baseball data
 test_that("Check the output of the Select function with baseball data is correct",{
   baseball.dataset <- paste(path.package("GA")[[1]],'/tests/baseball.dat',sep='')
   baseball.dat = read.table(baseball.dataset,header=TRUE)
   Niter <- 50
  output <- Select(baseball.dat, "salary", userfunc="AIC", user.family="gaussian", flag.log.scale=TRUE,
                   frac.replace=0.2, Niter=Niter, Nruns = 1, mutate.rate=FALSE, plot.flag=FALSE)

   #shouldn't we be looking at the sum of the absolute values of the residuals?
   expect_lt(sum(output$BestModel$residuals), 1e-14)
   expect_lt(output$BestFitness,541.0)
  })

set.seed(7)
test_that("Check the dimension of fitness matrix of the Select function with baseball data", {
  baseball.dataset <- paste(path.package("GA")[[1]],'/tests/baseball.dat',sep='')
  baseball.dat = read.table(baseball.dataset,header=TRUE)
  Niter <- 50
  output <- Select(baseball.dat, "salary", userfunc="AIC", user.family="gaussian", flag.log.scale=TRUE,
                   frac.replace=0.2, Niter=Niter, Nruns = 2, mutate.rate=FALSE, plot.flag=FALSE)
  
  ###################
  #I think output$fitness is a list contianing one element. So, I changed the way to access fitness matrix.

  expect_equal(dim((output$Fitness[[1]]))[1], 40)
  expect_equal(dim((output$Fitness[[2]]))[1], 40)
 
  expect_equal(dim((output$Fitness[[1]]))[2], Niter)
  expect_equal(dim((output$Fitness[[2]]))[2], Niter)
})

#test Select function with mtcars data
# test_that("Check the output of the Select function with mtcars data is correct",{
#   set.seed(8)
#   result <- Select(dataset=mtcars, response.name="mpg", user.family="gaussian", flag.log.scale=TRUE, Niter = 50, frac.replace = 0.2, mutate.rate = FALSE)
#     expect_equal(sum(result$bestModel$residuals), 6.938894e-18)
#     expect_equal(result$LastGen[[1]],
#                  c(1, 0, 1, 0, 1, 0, 0, 0, 0, 0))
#   })
#
# test_that("Check the dimension of fitness matrix of the Select function with mtcars data", {
#   set.seed(8)
#   result <- Select(dataset=mtcars, response.name="mpg", user.family="gaussian", flag.log.scale=TRUE, Niter = 50, frac.replace = 0.2, mutate.rate = FALSE)
#   expect_equal(dim(result$fitness)[1], 16)
#   expect_equal(dim(result$fitness)[2], 50)
# })
