#' Create a childe from two parents allowing for mutation
#'
#' \code{CrossOverMutate()} takes a \code{generation} and a list of index pairs
#' (\code{parent.index}) representing two parents selected from generation to mate
#' and produce 2 children.
#'
#' \emph{CrossOverMutate()} returns a two-item list of 'genomes' for each child.
#'
#' Called from within Breed()
#'
#' @inheritParams Select
#' @inheritParams Breed
#' @param parent.index A list of paired indices representig mating pairs of parents
#' @export
#' @examples
#'
#' \code{\link[GA]{Breed}}

CrossOverMutate <- function(generation, parent.index, prob.mutate=0.005){

  # Create child individual with half of its genetic material from parent1 and the other half from parent2
  # The generic material is chosen at random using sample
  parent1 <- generation[[parent.index[1]]]
  parent2 <- generation[[parent.index[2]]]
  child1 <- parent1
  child2 <- parent2

  # generate locations of genetic information to swap
  pos <- sample(length(parent1),as.integer(length(parent1)/2),replace=FALSE)
  child1[pos] <- parent2[pos]
  child2[pos] <- parent1[pos]

  # generate mutation vector
  mutate1 = rbinom(length(parent1),1,prob.mutate)
  mutate2 = rbinom(length(parent1),1,prob.mutate)

  # do the mutation - this will ensure that if a 2 is produced,
  # set to zero. If not, keeps as 1.
  child1 = (child1+mutate1)%%2
  child2 = (child2+mutate2)%%2
  child.pair = list(child1, child2)

  return(child.pair)
}
