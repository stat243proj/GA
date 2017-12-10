#' Remove genetic clones from a given generation
#'
#' \code{ReplaceClones()} removes any clones (genetically identical individuals) from a
#' given generation and replaces them with individuals randomly created from the entire
#' genome clones are predicted to exist when fitness of two individuals are exactly the
#' same this is highly unlikely unless they share the same exact genome.  Once idenfitied
#' clones are replaced with new individuals whoses genomes are randomly generated from a
#' binomial distribution with p = 0.5.  This is done to re-inject diversity back into the
#' existing gene pool.
#'
#' \emph{ReplaceClones()} produces a revised generation where all clones have been replaced
#'   by random individuals as described above.
#'
#' Called from within Select()
#'
#' @inheritParams Breed
#' @param C The number of genes (potential co-variantes) in an individual
#'
#' @export
#' @examples
#'
#' \code{\link[GA]{Select}}

ReplaceClones <- function(generation, fitness.vec) {

  Ncovar <- length(generation[[1]])
  clone.index <- which(duplicated(fitness.vec))
  N.clones <- length(clone.index)
  replacements <- lapply(1:N.clones, function(x) {rbinom(Ncovar,1,0.5)}) # list of new individual genomes
  generation[clone.index] <- replacements
  # the followin is to avoid computing fitness for the majority of non-clones
  fitness.replacements <- sapply(replacements, AssessFitness, response = response, predictors = predictors)
  dim(fitness.replacements)
  fitness.vec[clone.index] <- fitness.replacements
  output <- list("generation" = generation, "fitness" = fitness.vec)
  return(output)
}
