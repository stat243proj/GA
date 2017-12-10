#' Apply a genetic algorithm to find the optimal co-variates in a linear regression
#'
#' \code{Select()} is used to apply the genetic algorithm to some input dataset
#' outputs a list containing a list of the indiivdials in the final generation'
#' of the genetic algorithm, a matrix of the fitness values of all individuals in all
#' generations and the fitted model from glm for the best individual
#'
#' @param dataset A matrix, datatable, or dataframe
#' @param response.name The name of the column in \strong{dataset} that will act as the
#'   response variable to be predicted
#' @param userfunc A fitness function that operates on a model that could be provided by the user.
#'   The default is the Aikake Information Criteria or "AIC".
#' @param user.family Model family name to be passed to glm. Default is "gaussian"
#' @param flag.log.scale Default is TRUE if the log of the predictor varaible is to be fit.
#' @param frac.replace Fraction of worst parents to be replaced with the best children
#'   in each generation
#' @param Niter Maximum number of iterations. Default is 100
#' @param mutate.rate Genetic algorithm mutation rate. If set to FALSE it is automatically determined. A value of 0.01 #' is suggested
#' @param plot.flag Set to TRUE to plot the evolution of the population of individuals over the progression of the #' algorithm
#'
#' @keywords linear regression optimization genetic algorithm
#' @export
#' @examples
#' baseball = read.table(file.choose(),header=TRUE)
#' out <- Select(dataset=baseball, response.name="salary", Niter = 50, mutate.rate = 0.01)
#'
#' \code{\link[GA]{AssessFitness}}
#' \code{\link[GA]{Breed}}
#' \code{\link[GA]{CrossOverMutate}}
#' \code{\link[GA]{ExtractBestIndividual}}
#' \code{\link[GA]{ExtractResponseVariable}}
#' \code{\link[GA]{FitnessFunction}}
#' \code{\link[GA]{ReplaceClones}}

Select <- function(dataset, response.name, userfunc="AIC", user.family="gaussian", flag.log.scale=TRUE,
                   frac.replace=0.2, Niter=100, mutate.rate=FALSE, plot.flag=TRUE){

  #User can define a fitness function, log scale flag, fraction of children to replace with parents, number of iterations and a mutation rate. If these are not provided they are set to dafault

  # Define response and predictor variables
  subsets <- ExtractResponseVariable(dataset, response.name)

  # Choose to scale or reject bad data based on boolean flag

  if (flag.log.scale == TRUE) {
    response <<- log(subsets[[1]])
  } else {
    response <<- subsets[[1]]
  }
  predictors <<- subsets[[2]]

  # Define/create key variables a priori
  # These variables are accessible to all genalg functions

  C <- length(predictors) #Get the number of predictors (GLOBAL)
  Niter <<- Niter #number of iterations
  P <- as.integer(C*1.5) #number of individuals in a given generation (GLOBAL)
  P <<- 2*ceiling(P/2) # Force P to be even due to a future need to 'split' a generation in two

  #Set the mutation rate
  if (mutate.rate == FALSE) {
    prob.mutate <<- 1.0/(P*sqrt(C)) #mutation rate (should be about 1%) Formula suggested by G&H
  }
  else {
    prob.mutate <<- mutate.rate
  }

  fitness <<- matrix(0,P,Niter) #evolution of the fitness values over model run

  # Define first generation
  generation.old <<- lapply(1:P, function(x) {rbinom(C, 1, 0.5)}) # list of individual genomes

  #assess fitness of the first generation
  fitness[,1] <<- sapply(generation.old, AssessFitness, response = response, user.family, predictors = predictors, userfunc)

  # -------------------------------------------------------------------
  # MAIN LOOP for genetic algorithm
  # Loop through generations and apply selective forces to create iterative generations

  start <- Sys.time()

  for (n in 1:(Niter-1)) { #Niter -1 because we've already made the first generation

    # breed selection of P children and assess their fitness
    children <- Breed(generation.old, fitness[,n], prob.mutate)

    children.fitness <- sapply(children, AssessFitness, response = response, user.family, predictors = predictors, userfunc)
    number.children.keep <- round((1-frac.replace)*P)
    number.parents.keep <- P - number.children.keep

    # If we do want to keep parents in the new generation, then figure out the parents that we want to
    # keep. Otherwise, just replace all the parents with the children.
    if (number.parents.keep > 1){

      parents.fitness <- sapply(generation.old, AssessFitness, response = response, user.family, predictors = predictors, userfunc)

      children.best.index <- order(children.fitness)[1:number.children.keep] # select best children to keep
      children.best <- children[children.best.index] # select the children to keep
      children.fitness.best <- children.fitness[children.best.index] # select fitness of best children

      parents.best.index <- order(parents.fitness)[1:number.parents.keep] # get indices of best parents
      parents.best <- generation.old[parents.best.index] # select the parents to keep
      parents.fitness.best <- parents.fitness[parents.best.index] # select the fitness of the best parents

      #Create nre generation and new generation fitness by concatinating the vectors we made
      generation.new.fitness <- c(children.fitness.best,parents.fitness.best)
      generation.new <- c(children.best,parents.best)
    }

    else{
      generation.new <- children
      generation.new.fitness <- children.fitness
    }

    # check next generation for clones and replace them with random individuals if necessary
    clones.removed <- ReplaceClones(generation.new, generation.new.fitness, C)
    generation.new <- clones.removed$generation
    generation.new.fitness <- clones.removed$fitness

    #generation.old.worst.index <- which(rank(-fitness[,n])<=round(frac.replace*P)) # select worst parent by rank
    generation.old <- generation.new # keep most of prior generation
    fitness[,n+1] <- generation.new.fitness # keep most of prior generation fitness data

  }
  stop <- Sys.time()

  best.model <- ExtractBestIndividual(generation.new, generation.new.fitness, plot.flag)


  #If user wants to plot the evolution of the population over time, do so

  if (plot.flag == TRUE) {

    print(min(generation.new.fitness))
    plot(-fitness,xlim=c(0,Niter),ylim=c(min(fitness), max(fitness)),type="n",ylab="Negative fitness value",
         xlab="Generation",main="Fitness values For Genetic Algorithm")
    for(i in 1:Niter){points(rep(i,P),-fitness[,i],pch=20)}
  }


  # show run time
  cat("Algorithm runtime: ", round(as.numeric(stop-start),2), " seconds\n")
  # print(stop-start)

  # RETURN OUTPUT VARIABLES
  output <- list("LastGen" = generation.new, "fitness" = fitness, "bestModel" = best.model)
  return(output)

}
