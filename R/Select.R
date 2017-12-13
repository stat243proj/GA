#' Apply a genetic algorithm to find the optimal co-variates in a linear regression
#'
#' \code{Select()} is used to apply the genetic algorithm to some input dataset
#' outputs a list containing a list of the indiivdials in the final generation'
#' of the genetic algorithm, a matrix of the fitness values of all individuals in all
#' generations and the fitted model from glm for the best individual
#'
#' \strong{INPUTS}
#' @param dataset A matrix, datatable, or dataframe
#' @param response.name The name of the column in \strong{dataset} that will act as the
#'   response variable to be predicted
#' @param userfunc A fitness function that operates on a model that could be provided by the user.
#'   The default is the Aikake Information Criteria or "AIC".
#' @param user.family Model family name to be passed to \code{glm}. Default is "gaussian"
#' @param flag.log.scale Default is TRUE if the log of the predictor varaible is to be fit.
#' @param frac.replace Fraction of worst parents to be replaced with the best children
#'   in each generation
#' @param Niter Maximum number of iterations during each run. Default is 100
#' @param Nruns Number of times genetic algorithm is run.  Default is 1
#' @param mutate.rate Genetic algorithm mutation rate. If set to FALSE it is automatically 
#'   determined. A value of 0.01 is suggested
#' @param plot.flag Set to TRUE to plot the evolution of the population of individuals over the 
#'   progression of the algorithm
#' 
#' \strong{OUTPUTS}
#' \code{Select()} produces a single list each of whose elements are a list.vector of objects 
#'   partaining  to each of the \code{Nruns} times that the genetic algorithm was run.  Each 
#'   list element is named according the type of data it contains:
#' \itemize{
#'   \item \code{LastGen} is a list of the last generation of solutions produced during each run
#'   \item \code{Fitness} is a list of the complete fitness matrix representing all generations
#'     during each run
#'   \item \code{BestModel} is a list of the best model produced during each run
#'   \item \code{BestFitness} is a vector of the fitness of the best model from each run
#' }
#' 
#' @keywords linear regression optimization genetic algorithm
#' @export
#' @examples
#' baseball = read.table(file.choose(),header=TRUE)
#' out <- Select(dataset=baseball, response.name="salary", Niter=50, Nruns=1, mutate.rate = 0.01)
#'

Select <- function(dataset, response.name, userfunc=NA, user.family="gaussian", flag.log.scale=TRUE,
                   frac.replace=0.2, Niter=100, Nruns=1, mutate.rate=FALSE, plot.flag=TRUE){

  #User can define a fitness function, log scale flag, fraction of children to replace with parents, 
  # number of iterations and a mutation rate. If these are not provided they are set to dafault

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

  Ncovar <- length(predictors) #Get the number of predictors (GLOBAL)
  P <- 2*ceiling(as.integer(Ncovar*1.5)/2) #number of individuals in a given generation (GLOBAL)
  # Force P to be even due to a future need to 'split' a generation in two

  #Set the mutation rate
  if (mutate.rate == FALSE) {
    prob.mutate <- 1.0/(P*sqrt(Ncovar)) #mutation rate (should be about 1%) Formula suggested by G&H
  }
  else {
    prob.mutate <- mutate.rate
  }

  # ----------------------------------------------------------------------------------------------
  # PRIMARY LOOP for multiple runs of Genetic Algorithm
  
  # initiate master output variables outside Nruns
  Fitness <- as.list(rep(NA, Nruns))
  Last.Gen <- as.list(rep(NA, Nruns))
  Best.Model <- as.list(rep(NA, Nruns))
  Best.Fitness <- rep(NA, Nruns) # vector
  
  for (j in 1:Nruns) {
    
    # initiate subordinate (temporary) variables inside Nruns
    fitness <- matrix(0,P,Niter) #evolution of the fitness values over model run
    
    # Define first generation randomly from scratch
    generation.old <- lapply(1:P, function(x) {rbinom(Ncovar, 1, 0.5)}) # list of individual genomes
    
    #assess fitness of the first generation
    fitness[,1] <- sapply(generation.old, AssessFitness, response = response, user.family, predictors = predictors, userfunc)
    
    # ----------------------------------------------------------------------------------------------
    # SECONDARY LOOP for a single execution of the Genetic Algorithm
    # Loop through generations and apply selective forces to create iterative generations
    
    start.time <- Sys.time()
    
    for (n in 1:(Niter-1)) { #Niter -1 because we've already made the first generation
      
      # breed selection of P children and assess their fitness
      children <- Breed(generation.old, fitness[,n], prob.mutate)
      
      children.fitness <- sapply(children, AssessFitness, response = response, user.family, predictors = predictors, userfunc)
      number.children.keep <- round(frac.replace*P)
      number.parents.keep <- P - number.children.keep
      
      # If we do want to keep parents in the new generation, then figure out the parents that we want to
      # keep. Otherwise, just replace all the parents with the children.
      if (number.parents.keep > 1){
        
        parents.fitness <- fitness[,n]
        
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
      clones.removed <- ReplaceClones(generation.new, generation.new.fitness)
      generation.new <- clones.removed$generation
      generation.new.fitness <- clones.removed$fitness
      
      best.model.list <- ExtractBestIndividual(generation.new, generation.new.fitness)
      best.model <- best.model.list[[1]]
      best.individual <- best.model.list[[2]]
      best.fitness <- best.model.list[[3]]
      
      #generation.old.worst.index <- which(rank(-fitness[,n])<=round(frac.replace*P)) # select worst parent by rank
      generation.old <- generation.new # keep most of prior generation
      fitness[,n+1] <- generation.new.fitness # keep most of prior generation fitness data
      # print(paste("Best Fitness value in generation. :",n," ",round(best.fitness,3), sep=''))
    }
    
    # Assign subordinate output to master output
    Last.Gen[[j]] <- generation.new
    Fitness[[j]] <- fitness
    Best.Model[[j]] <- best.model
    Best.Fitness[j] <- best.fitness

    # show run time
    stop.time <- Sys.time()
    delt <- stop.time - start.time
    cat("Run #", j, " computation time", round(as.numeric(delt),2), units(delt), "\n")
  }
  
  if (plot.flag) {
    plot(-fitness,xlim=c(0,Niter),ylim=c(min(-fitness), max(-fitness)), type="n",
         ylab="Negative fitness values", xlab="Iteration", main="Fitness values For Genetic Algorithm")
    for(i in 1:Niter){
      points(rep(i,P), -fitness[,i], pch=20)
    }
  }

  # RETURN OUTPUT VARIABLES
  output <- list("LastGen" = Last.Gen, "Fitness" = Fitness, "BestModel" = Best.Model, "BestFitness" = Best.Fitness)
  return(output)

}
