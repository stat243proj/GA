#' Determine the fitness of some model
#'
#' This function takes a model object assesses its fitness
#' @param model A model output from lm or glm
#' @param userfunc A fitness function that operates on a model, provided by the user. Defaults to FALSE. Built in options include "Residual" or "BIC"
#' @keywords
#' @export
#' @examples
#' CrossOverMutate()

# -------------------------------------------------------------------
# CrossOverMutate
# Function that produces a single child from two chosen parents
# and allows for the possibility of mutation
CrossOverMutate <- function(generation, parent.index, prob.mutate){

  #Create child individual with half of its genetic material from parent1 and the other half from parent2
  #The generic material is chosen at random using sample
  parent1 <- generation[[parent.index[1]]]
  parent2 <- generation[[parent.index[2]]]

  child1 <- parent1
  child2 <- parent2
  #generate locations of genetic information to swap
  pos <- sample(1:length(parent2),as.integer(length(parent2)/2),replace=FALSE)
  child1[pos] <- parent2[pos]
  child2[pos] <- parent1[pos]

  #generate mutation vector
  mutate1 = rbinom(length(child1),1,prob.mutate)
  mutate2 = rbinom(length(child2),1,prob.mutate)
  #do the mutation - this will ensure that if a 2 is produced,
  #set to zero. If not, keeps as 1.
  child1 = (child1+mutate1)%%2
  child2 = (child2+mutate2)%%2
  child = data.frame(child1, child2)
  return(child)
}
