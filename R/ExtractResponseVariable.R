#' Extract response variable from dataframe
#'
#' This function allows the user to separate an input function
#' into reponse and predictor parts
#' @param datasets
#' @param name
#' @keywords
#' @export
#' @examples
#' ExtractResponseVariable()

ExtractResponseVariable <- function(dataset,name) {

  #Takes a dataframe, dataset and a name of a response variable
  #Extracts the response variable and dataframe of predictors, outputs these as members
  #of a list
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
