rfBlendReg <- function (formula, train, probeExp, probeRes, P, model, tuneLength) {
  require(randomForest)
  
  rfFit <- list()
  for(j in 1:tuneLength)  {
    rfFit[[j]] <- randomForest(formula, data = train, mtry = 2*j)
  }
  
  rfPredict <- list()
  for(j in 1:tuneLength)  {
    rfPredict[[j]] <- predict(rfFit[[j]], newdata = probeExp)
  }
  
  lmBlend <- list()  
  for(j in 1:tuneLength)  {
    lmBlend[[j]] <- blendUpdateReg(rfPredict[[j]], probeRes, P, model)
  }
  
  loss <- vector()
  for(j in 1:tuneLength)  {
  loss[j] <- lmBlend[[j]]$loss
  }
  minIndex <- which.min(loss)
  
  return(list(blendFinal = lmBlend[[minIndex]], modelFinal = rfFit[[minIndex]], predictFinal = rfPredict[[minIndex]]))
}

rfBlendClass <- function (formula, train, probeExp, probeRes, P, model, tuneLength) {
  require(randomForest)
  
  rfFit <- list()
  for(j in 1:tuneLength)  {
    rfFit[[j]] <- randomForest(formula, data = train, mtry = 2*j)
  }
  
  rfPredict <- list()
  for(j in 1:tuneLength)  {
    rfPredict[[j]] <- predict(rfFit[[j]], newdata = probeExp, type = "prob")[,2]
  }
  
  logiBlend <- list()  
  for(j in 1:tuneLength)  {
    logiBlend[[j]] <- blendUpdateClass(rfPredict[[j]], probeRes, P, model)
  }
  
  loss <- vector()
  for(j in 1:tuneLength)  {
    loss[j] <- logiBlend[[j]]$loss
  }
  minIndex <- which.min(loss)
  
  return(list(blendFinal = logiBlend[[minIndex]], modelFinal = rfFit[[minIndex]], predictFinal = rfPredict[[minIndex]]))
}