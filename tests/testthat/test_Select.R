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
                   frac.replace=0.2, Niter=Niter, Nruns = 1, mutate.rate=FALSE, plot.flag=TRUE)

   #output$BestModel is a list containing one element
   expect_lt(sum(output$BestModel[[1]]$residuals), 1e-14)
   expect_lt(output$BestFitness,541.0)
  })

set.seed(7)
test_that("Check the dimension of fitness matrix of the Select function with baseball data", {
  baseball.dataset <- paste(path.package("GA")[[1]],'/tests/baseball.dat',sep='')
  baseball.dat = read.table(baseball.dataset,header=TRUE)
  Niter <- 50
  #Note use of Nruns = 2 here
  output <- Select(baseball.dat, "salary", userfunc="AIC", user.family="gaussian", flag.log.scale=TRUE,
                   frac.replace=0.2, Niter=Niter, Nruns = 2, mutate.rate=FALSE, plot.flag=TRUE)

  ###################

  expect_equal(dim((output$Fitness[[1]]))[1], 40)
  expect_equal(dim((output$Fitness[[2]]))[1], 40)

  expect_equal(dim((output$Fitness[[1]]))[2], Niter)
  expect_equal(dim((output$Fitness[[2]]))[2], Niter)
})
