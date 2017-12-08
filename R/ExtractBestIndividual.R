#' Determine the fitness of some model
#'
#' Called from within GeneticAlgorithmFit
#' @param generation
#' @param fitnessmatrix
#' @keywords
#' @export
#' @examples
#' ExtractBestIndividual(generation,fitnessmatrix)


# -------------------------------------------------------------------
# ExtractBestIndividual
# Function that extracts the best individual and its corresponding fitness, and prints a set of
# summary statistics
ExtractBestIndividual <- function(generation, fitnessmatrix){

  #Extract the best individual and its corresponding fitness, and print a set of
  #summary statistics

  best.index <- order(fitnessmatrix[,Niter])[1]

  best.individual <- generation[[best.index]]
  print(best.individual)
  predictors.individual <- predictors[,best.individual==1]
  best.model <- lm(response[,1]~., predictors.individual)
  print(summary(best.model))
  print(best.individual)
  return(best.model)
}
