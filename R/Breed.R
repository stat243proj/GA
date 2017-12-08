#' Determine the fitness of some model
#'
#' This function takes a model object assesses its fitness
#' @param model A model output from lm or glm
#' @param userfunc A fitness function that operates on a model, provided by the user. Defaults to FALSE. Built in options include "Residual" or "BIC"
#' @keywords
#' @export
#' @examples
#' Breed()


  # Breed
  # Function that breeds P new children based on parents' genome and fitness

  Breed <- function(generation, fitness.vec, predictors, prob.mutate) {

    # generation is a list with each element containing the genome of an individual
    # fitness.vec is a vector
    prob.reproduction <- 2*rank(-fitness.vec)/(P*(P+1))
    parent.index.list <- lapply(1:(P/2), function(x) sample(P,2,prob = prob.reproduction,replace=FALSE))

    children <- lapply(parent.index.list, function(x) CrossOverMutate(generation, x, prob.mutate))

    # return P children to be considered for selection
    # also return fitness evaluation

    return(children)
  }
