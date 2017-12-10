#' Create an ensemble of 'children' from a given generation of parents
#'
#' \code{Breed()} takes a \code{generation} and its associated fitness data
#'   in \code{fitness.vec} as input and produces a number of children equal to the number
#'   of parents in \code{generation}.  This function allows for a natural rate of
#'   genetic mutation as set by \code{prob.mutate}.
#'
#' \emph{Breed()} returns a list of 'genomes' for each child.
#'
#' Called from within Select()
#'
#' @param generation A list of individual genomes or co-variate sets
#' @param fitness.vec A vector of the fitness values for the input \code{generation}
#' @param prob.mutate A natural rate of gene mutation determined in \code{\link{Select.R}}
#'   or fixed by the user.
#' @export
#' @examples
#'
#' \code{\link[GA]{CrossOverMutate}}

Breed <- function(generation, fitness.vec, prob.mutate) {

  # generation is a list with each element containing the genome of an individual
  # fitness.vec is a vector
  prob.reproduction <- 2*rank(-fitness.vec)/(P*(P+1))
  parent.index.list <- lapply(1:P, function(x) sample(P,2,prob = prob.reproduction,replace=FALSE))

  child.pairs <- lapply(parent.index.list, function(x) CrossOverMutate(generation, x, prob.mutate))
  # collate list of P child pairs into a single generation (list) of P children
  children <- unlist(child.pairs, recursive = FALSE)[1:P]
  # children <- lapply(child.pairs, function(x) x[[1]])

  return(children)
}
