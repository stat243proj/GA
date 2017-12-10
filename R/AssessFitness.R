#' Quantify the 'fitness' of a given linear model
#'
#' \code{AssessFitness()} determines 'fitness' of an invidivudal based on the quality
#' of least squares fit of . The default for determining fitness is the Aikake Criteria Index
#' but the user can supply their own custom-made fitness function that takes a linear
#' model output object as its input.
#'
#' \emph{AssessFitness()} returns a single fitness value.
#'
#' Called from within Select()
#'
#' @param individual A binary vector of length C = # of potential co-variates in an individual.
#'   Co-variates to be used in the regression of \code{response} versus \code{predictors} are
#'   indicated by ones while the co-variates that are exlcuded are indicated by zeros.
#' @param response The vector of response data (Y)
#' @param predictors A data table or data frame of the co-variates (X)
#' @inheritParams Select
#' @export
#' @examples
#'
#' \code{\link[GA]{FitnessFunction}}
#' \code{\link{glm}}
#' \code{\link{extractAIC}}

AssessFitness <- function(individual, response, predictors, user.family="gaussian", userfunc="AIC"){

  #Evaluate the fitness of some model, output from glm
  #The userfunc should take a fitted model and output a scalar fitness value

  #ensure that there is more than one '1' in the vector
  while (sum(individual) <= 1){

    insertonepos = sample(1:length(individual),1)
    individual[insertonepos] = 1
  }
  #print(individual)

  predictors.individual <- predictors[,individual==1]

  #Check distribution family of glm()
  if(!user.family %in% c("binomial", "gaussian", "Gamma", "poisson", "inverse.gaussian")){
    print(paste("WARNING: User defined distribution family ", user.family, " does not exist", sep=''))
    stop()
    geterrmessage()
  }
  else{
    model.out <- glm(response[,1]~., family=user.family, data=predictors.individual)
    #model.out <- lm(response[,1]~., predictors.individual)
    fitness.value <- FitnessFunction(model.out, userfunc)
  }

  return(fitness.value)
}
