#' Determine the fitness of the result of a given linear model
#'
#' \code{FitnessFunction()} determines the 'fitness' of a given linear fit using either a
#' function supplied by the user or the Aikake Information Criteria \code{\link{extractAIC}}
#' as a default.  In either case, the fitness function is designed to specifically take
#' a linear regression object produced by either \code{\link{lm}} or \code{\link{glm}}.
#'
#' \emph{FitnessFunction()} returns a single scalar fitness value
#'
#' Called from within AssessFitness()
#'
#' @param model The object produced by the built-in R regression function \code{\link{lm}} or \code{\link{glm}}
#' @inheritParams Select
#' @export
#' @examples
#'
#' \code{\link[GA]{AssessFitness}}
#' \code{\link{glm}}
#' \code{\link{lm}}
#' \code{\link{extractAIC}}
FitnessFunction <- function(model, userfunc){
  
  if ((class(userfunc) == "character")&&(userfunc == "AIC")) { # default case of Aikake Info Criteria
    
    fitness.value <- extractAIC(model)[2]
    return(fitness.value)
    
  } 
  
  if (class(userfunc) == "function") { # test if submitted function is a function object
      
      fitness.value = userfunc(model)
      return(fitness.value)
      
    } 
  else {
      print(paste0("WARNING: ", deparse(substitute(unserfunc)), "() is not a recognized function"))
    }
  }
