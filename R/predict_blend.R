#' @title Prediction function for blend objects 
#' 
#' @description
#' Predicts on new data using a blend object
#' 
#' @details
#' For regression a numeric vector of predicted values is outputted.  For classification a dataframe is outputted, with first
#' column the predicted probabilities and second column the predicted classes.   
#' 
#' @param blendObj the fitted blend object
#' @param testX the dataframe of new explanatory (i.e. "X") variables to predict on, must exactly match the structure of the 
#' data frame used to fit the blend object
#' @return a numeric vector in the regression case, a dataframe in the classification case
#' @author Alex Ustian <alex.l.ustian@@gmail.com>
#' @export
#' @examples
#' data(mpgData)
#' modelVec <- c("gbm", "svm", "lm")
#' blendReg <- blend(mpg~., data = mpgData, models = modelVec, tuneLength = 5, folds = 10, repeats = 3)
#' predict(blendReg, mpgData)
#' 
#' data(carData)
#' modelVec <- c("gbm", "svm", "logi")
#' blendClass <- blend(V7~., data = carData, models = modelVec, tuneLength = 5, folds = 5, repeats = 2)
#' predict(blendClass, carData)
predict.blend <- function(blendObj, testX)  {
  modelsReg <- names(blendObj)[2:(length(names(blendObj))-1)]
  modelsClass <- names(blendObj)[3:(length(names(blendObj))-1)]
  predict <- list()
  
#predicting on the testX set with the model chosen to be used in the blend
#different calls to the individual models are required for the classification and regression cases
  bool <- (length(class(blendObj$blend)) == 1)
  
  #regression
  if(bool)  {
    for(i in 1:length(modelsReg))  {
      predict[[modelsReg[[i]]]] = switch(modelsReg[[i]], 
           rpart = predict(blendObj$rpart, newdata = testX),
           lm = predict(blendObj$lm, newdata = testX),
           ctree = predict(blendObj$ctree, newdata = testX, type = "response"),
           nnet = {predVec = predict(blendObj$nnet, newdata = testX)    #not properly scaled
                   blendObj$nnet$max*predVec + blendObj$nnet$min},
           gbm = predict(blendObj$gbm, newdata = testX, n.trees = length(blendObj$gbm$train.error)),
           enet = {predMat = predict(blendObj$enet, newx = testX, type = "fit")   #this is a matrix
                   predMat$fit[, blendObj$enet$step]},  
           pls = {predMat = predict(blendObj$pls, newdata = testX)   #this is a matrix
                  predMat[,1,blendObj$pls$comp]},
           pcr = {predMat = predict(blendObj$pcr, newdata = testX)  #this is a matrix
                  predMat[,1,blendObj$pcr$comp]},
           earth = predict(blendObj$earth, newdata = testX),
           rf = predict(blendObj$rf, newdata = testX),
           svm = predict(blendObj$svm, newdata = testX))
    }
    
    #combining the testX predictions and using the blended linear model to predict on the testX set
    int <- matrix(1, dim(testX)[1], 1)
    blendPredictDf <- cbind.data.frame(int, predict)
    colnames(blendPredictDf) <- c("int", modelsReg)
    return(predict(blendObj$blend, newdata = blendPredictDf))
  }
  
  #classification
  else{
    for(i in 1:length(modelsClass))  {
      predict[[modelsClass[[i]]]] = switch(modelsClass[[i]], 
          rpart = predict(blendObj$rpart, newdata = testX, type = "prob")[,2],
          logi = predict(blendObj$logi, newdata = testX, type = "response"),
          ctree = sapply(predict(blendObj$ctree, newdata = testX, type = "prob"), function(x) x[2]),
          nnet = predict(blendObj$nnet, newdata = testX),    
          gbm = predict(blendObj$gbm, newdata = testX, n.trees = length(blendObj$gbm$train.error), type = "response"),
          enet = {predMat = predict(blendObj$enet, newx = testX, type = "fit")   #this is a matrix
                  predMat$fit[, blendObj$enet$step]},  
          pls = {predMat = predict(blendObj$pls, newdata = testX)   #this is a matrix
                 predMat[,1,blendObj$pls$comp]},
          earth = predict(blendObj$earth, newdata = testX, type = "response"),
          rf = predict(blendObj$rf, newdata = testX, type = "prob")[,2],
          svm = predict(blendObj$svm, newdata = testX, type = "probabilities")[,2])
    }

    #combining the testX predictions and using the blended logistic model to predict on the testX set
    int <- matrix(1, dim(testX)[1], 1)
    blendPredictDf <- cbind.data.frame(int, predict)
    colnames(blendPredictDf) <- c("int", modelsClass)
    
    probPreds <- predict(blendObj$blend, newdata = blendPredictDf, type = "response")
    factorPreds <- factor(ifelse(probPreds < 0.5, blendObj$levels[1], blendObj$levels[2]))
    return(data.frame(probPreds, factorPreds))   
  }
}