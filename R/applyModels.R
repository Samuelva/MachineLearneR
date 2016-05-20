#' @title  Applying all MachineLearneR models in parallel
#' @description This function is used internally inside the ML function to apply all sample-generated models on the entire dataset
#' @param i An integer, specifying nth parallel iteration
#' @param files A vector containing the filenames of the subsets
#' @param analyticalVariables A vector containing the names of the variables in the dataset that should be analysed
#' @param selectedMissingData A string specifying the method for imputing missing data
#' @param selectedNormalization A string specifying the method for normalizing the data
#' @param metaVariables A vector containing the names of the variables which should not be analysed, but should still remain in the dataset as describing variables
#' @param selectedTransformation A string specifying the method for transforming the data
#' @param selectedStandardization A string specifying the method for standardizing the data
#' @param splitCol A string specifying the name of the variable on which the dataset is splitted in subsets
#' @param classifierClass A string specifying the name of the variable on which the dataset should be classified
#' @param removeCata A boolean indicating if categorical variables should be removed from the dataset
#' @param factorList A list containing the factor scores, previously calculated on the sample dataset
#' @param faMethodScores A string describing the method used for calculating the factors
#' @param selectedAverage A string describing the basis for data normalization, can be mean, median or modus
#' @param removeOutliers A boolean indicating if outliers should be removed in data normalization
#' @param controlVariable A string specifying the variable on which the data should be normalized
#' @param controlValue A string specifying the value of the controlVariable on which the data should be normalized
#' @param classModel A randomforest fit object, to be applied to the data
#' @param skew A list containing the sample-calculated skewness per variable
#' @param kurto A list containing the sample-calculated kurtosis per variable
#' @param selectedTrainingSize An integer specifying the percentage of the dataset that should be used as trainingset for training the random forest model
#' @param createPlots A boolean indicating if plots should be generated
#' @param factors A vector containing the names of the sample-calculated factors, to be used as new analytical variables
#' @export

applyModels <- function(i, files, analyticalVariables, selectedMissingData,
                     selectedNormalization, metaVariables, selectedTransformation,
                     selectedStandardization, splitCol, classifierClass,
                     removeCata, factorList, faMethodScores, selectedAverage,
                     removeOutliers,controlVariable,controlValue, classModel,
                     skew, kurto,selectedTrainingSize, createPlots, factors) {
  # i is a index for the vector containing the subsset files.
  require("pryr")
  load(files[i])

  ## Apply data manipulation model
  print(paste('xSet ', (object.size(xSet)/1024)/1024," mb used.", sep=''))
  print(paste("Voor DMM ", (mem_used()/1024)/1024," mb used.", sep=''))

  varList <- dMM(mydata=xSet, analyticalVariables = analyticalVariables, selectedNormalization=selectedNormalization,
                   selectedAverage=selectedAverage, selectedTransformation=selectedTransformation, selectedStandardization=selectedStandardization, splitCol=splitCol,
                   removeOutliers=removeOutliers, controlVariable=controlVariable, controlValue=controlValue, multiThreadFase=T, skew = skew, kurto = kurto)


  varList$mydata <- data.frame(varList$mydata, xSet)
  colnames( varList$mydata ) <- gsub(".1", ".raw", colnames( varList$mydata ), fixed = T)
  rm(xSet)
  gc()
  mallinfo::malloc.trim()
  print(paste("Na DMM ", (mem_used()/1024)/1024," mb used.", sep=''))
  #
  #
  # ## Apply multiple imputation model
  if(any(is.na(varList$mydata))){
    varList$mydata <- cMIM(mydata=varList$mydata,analyticalVariables=analyticalVariables, selectedMissingData=selectedMissingData,
                      metaVariables=metaVariables, classifierClass=classifierClass, removeCata=removeCata)
  }
  # print(paste("Na imputen ", (mem_used()/1024)/1024," mb used.", sep=''))
  # ## Apply factor loading model
  # #varList$mydata[,-metaVariables] <- applyFactorLoadingModel(varList$mydata[,-metaVariables], covlist=factorList$covList, solution=factorList, factorNames=factorList$factorNames)
  varList$mydata <- data.frame( varList$mydata[,metaVariables], applyFactorLoadingModel(varList$mydata[,analyticalVariables],
                                  solution=factorList, factorNames=factorList$factorNames ,faMethodScores=faMethodScores))

  print(paste("Na fa ", (mem_used()/1024)/1024," mb used.", sep=''))


  #### Maak eeen optie om de analytical vars te onthouden defeault is false
  colnames( varList$mydata ) <- gsub("X", "Component", colnames( varList$mydata ), fixed = T)
  analyticalVariables <- names(varList$mydata)[!(names(varList$mydata) %in% metaVariables)]
  modelFit <- createClassModel(mydata=varList$mydata, selectedTrainingSize=selectedTrainingSize, classifierClass=classifierClass,
                   analyticalVariables=factors ,  createPlots=createPlots, fit=classModel, parallelIter = i, cores=1, multiThreadFase=T)

  print(paste("Na class ", (mem_used()/1024)/1024," mb used.", sep=''))


}

