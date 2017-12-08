#' Determine the fitness of some model
#'
#' Called from within GeneticAlgorithmFit
#' @param model A model output from lm or glm
#' @param userfunc A fitness function that operates on a model, provided by the user. Defaults to FALSE. Built in options include "Residual" or "BIC"
#' @keywords
#' @export
#' @examples
#' FitnessFunction()


# FitnessFunction
#Evaluate the fitness of some model, output from lm or glm
#The userfunc should take a fitted model and output a scalar
#fitness value

FitnessFunction <- function(model, userfunc=FALSE){

  if (userfunc == FALSE) {
    fitness.value <- extractAIC(model)[2]
  }

  else if (userfunc == "Residual"){
    fitness.value <- sum(model$residuals^2)
  }

  else if(userfunc == "BIC"){
    fitness.value <- BIC(model)
  }
  else{
    print(paste("WARNING: User Fitness Function ", userfunc, " cannot be calculated", sep=''))
    fitness.value = NULL
  }
  return(fitness.value)
}
