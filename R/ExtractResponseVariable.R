#' Extract response variable from dataframe
#'
#' \code{ExtractResponseVariable()} allows the user to take a data table or dataframe
#' \code{dataset} and extract a 'response' vector indicated by \code{response.name}.  All
#' remaining columns are collated into a matrix of co-variates or 'predictors'.
#'
#' \emph{ExtractResponseVariable()} returns the response vector and predictor matrix as a
#'   two-item list.
#'
#' Called from within Select()
#'
#' @inheritParams Select
#' @export
#' @examples
#'
#' \code{\link[GA]{Select}}
#'

ExtractResponseVariable <- function(dataset, name){
  if (!is.data.frame(dataset)){
    print("WARNING: The input data is not a data frame")
    stop()
    geterrmessage()
   }
  
  #Check if column values are numeric
  if(sum(!(sapply(dataset[, -which(names(dataset)==name)], class) %in% c("numeric","integer"))>0)){
    print("WARNING: There exists certain non-numeric predictor(s)")
    stop()
    geterrmessage()
  }
  
  #If there are missing values in response variable, remove the observation and proceed the function
  if(any(is.na(dataset[name]))){
    print("The response variable has missing values")
    dataset <- dataset[!is.na(dataset[name])]
  }
  
  #Check if the response variable is in the dataset
  if(name %in% colnames(dataset)) {
    
    #Get matrix of predictors
    predictors <- dataset
    predictors[name] <- NULL
    
    #Get response variable
    response <- dataset[name]
    return(list(response,predictors))
  } 
  
  else {
    print(paste("Response variable ",name," not found in dataset",sep=''))
    return(list(0L,0L))
  }
}
