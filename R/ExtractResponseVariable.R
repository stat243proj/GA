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
# function to get separate response variable from predictors
#Changes made by Kexin Fei, Yachen Wang, Shan Gao
ExtractResponseVariable <- function(dataset,name) {
  #Takes a dataframe, dataset and a name of a response variable
  #Extracts the response variable and dataframe of predictors, outputs these as members 
  #of a list
  
  if(sum(!(sapply(dataset[, -which(names(dataset)==name)], class) %in% c("numeric", "integer"))>0)){
    print("WARNING: There exists certain non-numeric predictor(s)")
    stop()
    geterrmessage()
  }
  
  #if there are missing values in response variable, remove the observation and proceed the function
  if (any(is.na(dataset[name]))){
      print("The response variable has missing values")
      dataset <- dataset[!is.na(dataset[name])]
    }
  
  if (name %in% colnames(dataset)) {
    #Get matrix of predictors
    predictors <- dataset
    predictors[name] <- NULL
        
    #Get response variable
    response <- dataset[name]
    return(list(response,predictors))
  } 
  
  else {
    print(paste("Name ",name," not found in dataset",sep=''))
    return(list(0L,0L))
  }
}


