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
#'\code{\link[GA]{Select}}
#'

ExtractResponseVariable <- function(dataset, response.name) {

  if (any(is.na(dataset[,1]))==TRUE){
    print("The response variable has missing values")
    dataset <- dataset[!is.na(dataset[,1])]
  }
  else{
    if (name %in% colnames(dataset)) {

      name <- as.character(name)

      #Get matrix of predictors
      predictors <- dataset
      predictors[name] <- NULL

      #Get response variable
      response <- dataset[name]
      return(list(response,predictors))
    } else {
      print(paste("Name ",name," not found in dataset",sep=''))
      return(list(0L,0L))
    }
  }
}


