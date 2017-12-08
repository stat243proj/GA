#' Determine the fitness of some model
#'
#' This function takes a model object assesses its fitness
#' @param model A model output from lm or glm
#' @param userfunc A fitness function that operates on a model, provided by the user. Defaults to FALSE. Built in options include "Residual" or "BIC"
#' @keywords
#' @export
#' @examples
#' ReplaceClones()


# -------------------------------------------------------------------
# RemoveClones
# function that removes any clones from a given generation and replaces them
# with individuals randomly created from the entire genome clones are predicted
# to exist when fitness of two individuals are exactly the same
# this is highly unlikely unless they share the same exact genome

ReplaceClones <- function(generation, fitness.vec, C) {

  # function that removes any clones from a given generation and replaces them
  # with individuals randomly created from the entire genome
  # clones are predicted to exist when fitness of two individuals are exactly the same
  # this is highly unlikely unless they share the same exact genome
  clone.index <- which(duplicated(fitness.vec))
  N.clones <- length(clone.index)
  replacements <- lapply(1:N.clones, function(x) {rbinom(C,1,0.5)}) # list of new individual genomes
  generation[clone.index] <- replacements
  # the followin is to avoid computing fitness for the majority of non-clones
  fitness.replacements <- sapply(replacements, AssessFitness, response = response, predictors = predictors, userfunc = FALSE)
  dim(fitness.replacements)
  fitness.vec[clone.index] <- fitness.replacements
  output <- list("generation" = generation, "fitness" = fitness.vec)
  return(output)
}
