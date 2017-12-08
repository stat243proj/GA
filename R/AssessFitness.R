#' Determine the fitness of some model
#'
#' Called from within GeneticAlgorithmFit
#' @param individual
#' @param response
#' @param user.family
#' @param predictors
#' @param userfunc A fitness function that operates on a model, provided by the user. Defaults to FALSE. Built in options include "Residual" or "BIC"
#' @keywords
#' @export
#' @examples
#' AssessFitness()



# -------------------------------------------------------------------
# AssessFitness
# function that determines 'fitness' of an invidivudal based on the quality
# of the LS fit. The default for determining fitness is the Aikake Criteria Index
# but the user can supply their own custom-made fitness function
# **may be worth it to treat 'predictors' as global variable or object

AssessFitness <- function(individual, response, user.family="gaussian", predictors, userfunc=FALSE){
  
  #Evaluate the fitness of some model, output from glm
  #The userfunc should take a fitted model and output a scalar fitness value
  
  #if model selects one or zero predictors, then add 2 predictors to the model
  if((sum(individual) == 0) || (sum(individual) == 1)){
    individual[sample(1:length(individual), 2)] = 1
  }
  
  predictors.individual <- predictors[,individual==1]
  
  #Check distribution family of glm()
  if(!user.family %in% c("binomial", "gaussian", "Gamma", "poisson", "inverse.gaussian")){
    print(paste("WARNING: User defined distribution family ", user.family, " is not existed", sep=''))
    fitness.value = NULL
  }
  else{
    model.out <- glm(response[,1]~., family=user.family, predictors.individual)
    fitness.value <- FitnessFunction(model.out, userfunc)
  }
  return(fitness.value)
}

