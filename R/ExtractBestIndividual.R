#' Indentify the individual with the best fitness in a given generation
#'
#' \code{ExtractBestIndividual()}
#' \emph{ExtractBestIndividual()} identifies the most fit individual within a
#' \code{generation} and returns linear regression object produced by either
#' \code{\link{lm}} or \code{\link{glm}}.  It also prints a summary of the linear
#' regression object and the fitness value if prompted by the user.
#'
#' Called from within Select()
#'
#' @inheritParams Breed
#' @param plot.flat A binary flag (0/1) which indicates whether to print a summary of the fitness
#'   of the best individual
#' @export
#' @examples
#'
#' \code{\link[GA]{Select}}
#' \code{\link{glm}}
#' \code{\link{lm}}

ExtractBestIndividual <- function(generation, fitness.vec){

  #Extract the best individual and its corresponding fitness, and print a set of
  #summary statistics

  best.index <- order(fitness.vec)[1]

  best.individual <- generation[[best.index]]
  predictors.individual <- predictors[,best.individual==1]
  best.model <- lm(response[,1]~., predictors.individual)
  best.fitness <- fitness.vec[best.index]
  return(list(best.model,best.individual,best.fitness))
}
