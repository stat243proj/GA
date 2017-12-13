#Test the FitnessFunction
data(mtcars)
C <- dim(mtcars)[2] - 1
P <- as.integer(C*1.5)
P <- 2*ceiling(P/2)
prob.mutate <- 1.0/(P*sqrt(C))
context("Test the output of FitnessFunction")
test_that("Check the return fitness value from FitnessFunction is 163",{
  data("mtcars")
  subsets <- ExtractResponseVariable(mtcars, "mpg")
  response <- subsets[[1]]
  predictors.individual <- subsets[[2]]
  model <- glm(response[,1]~., family="gaussian", data=predictors.individual)
  aic <- floor(model$aic)
  userfunc <- "AIC"
  expect_equal(floor(FitnessFunction(model, userfunc)), aic)
})

