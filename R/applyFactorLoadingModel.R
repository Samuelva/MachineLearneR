#' @title  Applying factor loading model in parallel
#' @description This function is used internally for applying the factor loading model inside the function; \code{\link{applyModels}}
#' @param mydata A dataframe containing the all the initial analytical variables
#' @param solution A list containing the factor scores, previously calculated on the sample dataset
#' @param factorNames A vector containing the names of the factors
#' @param faMethodScores A string describing the method used for calculating the factors
#' @export
applyFactorLoadingModel <- function(mydata, solution, factorNames, faMethodScores){
  mydataMatrix <- as.matrix(mydata)

  covx <- rrcov::Cov(mydataMatrix)

  covList <- list(cov = rrcov::getCov(covx), center = rrcov::getCenter(covx))

  dfFactorScores <- data.frame(robustfa::computeScores(solution, x = mydataMatrix,
                               covmat = covList, cor = TRUE, scoresMethod = faMethodScores)$scores)
  colnames(dfFactorScores) <- factorNames
#trest
  #test2
  #test3
  return(dfFactorScores)

}
