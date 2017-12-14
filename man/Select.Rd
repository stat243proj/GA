\name{Select}
\alias{Select}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Select(dataset, response.name, userfunc = "AIC", user.family = "gaussian", flag.log.scale = TRUE, frac.replace = 0.2, Niter = 100, Nruns = 1, mutate.rate = FALSE, plot.flag = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{
%%     ~~Describe \code{dataset} here~~
}
  \item{response.name}{
%%     ~~Describe \code{response.name} here~~
}
  \item{userfunc}{
%%     ~~Describe \code{userfunc} here~~
}
  \item{user.family}{
%%     ~~Describe \code{user.family} here~~
}
  \item{flag.log.scale}{
%%     ~~Describe \code{flag.log.scale} here~~
}
  \item{frac.replace}{
%%     ~~Describe \code{frac.replace} here~~
}
  \item{Niter}{
%%     ~~Describe \code{Niter} here~~
}
  \item{Nruns}{
%%     ~~Describe \code{Nruns} here~~
}
  \item{mutate.rate}{
%%     ~~Describe \code{mutate.rate} here~~
}
  \item{plot.flag}{
%%     ~~Describe \code{plot.flag} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dataset, response.name, userfunc = "AIC", user.family = "gaussian", 
    flag.log.scale = TRUE, frac.replace = 0.2, Niter = 100, Nruns = 1, 
    mutate.rate = FALSE, plot.flag = TRUE) 
{
    subsets <- ExtractResponseVariable(dataset, response.name)
    if (flag.log.scale == TRUE) {
        response <<- log(subsets[[1]])
    }
    else {
        response <<- subsets[[1]]
    }
    predictors <<- subsets[[2]]
    Ncovar <- length(predictors)
    P <- 2 * ceiling(as.integer(Ncovar * 1.5)/2)
    if (mutate.rate == FALSE) {
        prob.mutate <- 1/(P * sqrt(Ncovar))
    }
    else {
        prob.mutate <- mutate.rate
    }
    Fitness <- as.list(rep(NA, Nruns))
    Last.Gen <- as.list(rep(NA, Nruns))
    Best.Model <- as.list(rep(NA, Nruns))
    Best.Individual <- matrix(0, Nruns, Ncovar)
    Best.Fitness <- rep(NA, Nruns)
    for (j in 1:Nruns) {
        print(paste("Run ", j, " of genetic algorithm", sep = ""))
        fitness <- matrix(0, P, Niter)
        generation.old <- lapply(1:P, function(x) {
            rbinom(Ncovar, 1, 0.5)
        })
        fitness[, 1] <- sapply(generation.old, AssessFitness, 
            response = response, user.family, predictors = predictors, 
            userfunc)
        start.time <- Sys.time()
        for (n in 1:(Niter - 1)) {
            children <- Breed(generation.old, fitness[, n], prob.mutate)
            children.fitness <- sapply(children, AssessFitness, 
                response = response, user.family, predictors = predictors, 
                userfunc)
            number.children.keep <- round(frac.replace * P)
            number.parents.keep <- P - number.children.keep
            if (number.parents.keep > 1) {
                parents.fitness <- fitness[, n]
                children.best.index <- order(children.fitness)[1:number.children.keep]
                children.best <- children[children.best.index]
                children.fitness.best <- children.fitness[children.best.index]
                parents.best.index <- order(parents.fitness)[1:number.parents.keep]
                parents.best <- generation.old[parents.best.index]
                parents.fitness.best <- parents.fitness[parents.best.index]
                generation.new.fitness <- c(children.fitness.best, 
                  parents.fitness.best)
                generation.new <- c(children.best, parents.best)
            }
            else {
                generation.new <- children
                generation.new.fitness <- children.fitness
            }
            clones.removed <- ReplaceClones(generation.new, generation.new.fitness)
            generation.new <- clones.removed$generation
            generation.new.fitness <- clones.removed$fitness
            best.model.list <- ExtractBestIndividual(generation.new, 
                generation.new.fitness)
            best.model <- best.model.list[[1]]
            best.individual <- best.model.list[[2]]
            best.fitness <- best.model.list[[3]]
            generation.old <- generation.new
            fitness[, n + 1] <- generation.new.fitness
        }
        Last.Gen[[j]] <- generation.new
        Fitness[[j]] <- fitness
        Best.Model[[j]] <- best.model
        Best.Fitness[j] <- best.fitness
        Best.Individual[j, ] <- best.individual
        stop.time <- Sys.time()
        delt <- stop.time - start.time
        cat("Run #", j, " Computation Time", round(as.numeric(delt), 
            2), units(delt), "\n")
        cat("Run #", j, " Final Model Summary: ", "\n")
        print(summary(Best.Model[[j]]))
    }
    if (plot.flag) {
        plot(-fitness, xlim = c(0, Niter), ylim = c(min(-fitness), 
            max(-fitness)), type = "n", ylab = "Negative fitness values", 
            xlab = "Iteration", main = "Fitness values For Genetic Algorithm")
        for (i in 1:Niter) {
            points(rep(i, P), -fitness[, i], pch = 20)
        }
    }
    output <- list(LastGen = Last.Gen, Fitness = Fitness, BestModel = Best.Model, 
        BestFitness = Best.Fitness, BestIndividual = Best.Individual)
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
