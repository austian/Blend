svmBlendReg <- function (formula, train, probeExp, probeRes, P, model, tuneLength) {
  require(kernlab)
  
  svmFit <- list()
  for(j in 1:tuneLength)  {
    svmFit[[j]] <- ksvm(formula, data = train, kernel="rbfdot", C=2^(-2 + j))
  }
  
  svmPredict <- list()
  for(j in 1:tuneLength)  {
    svmPredict[[j]] <- predict(svmFit[[j]], newdata = probeExp)
  }
  
  lmBlend <- list()
  for(j in 1:tuneLength)  {
    lmBlend[[j]] <- blendUpdateReg(svmPredict[[j]], probeRes, P, model)
  }
  
  loss <- vector()
  for(j in 1:tuneLength)  {
  loss[j] <- lmBlend[[j]]$loss
  }
  minIndex <- which.min(loss)
  
  return(list(blendFinal = lmBlend[[minIndex]], modelFinal = svmFit[[minIndex]], predictFinal = svmPredict[[minIndex]]))
}

svmBlendClass <- function (formula, train, probeExp, probeRes, P, model, tuneLength) {
  require(kernlab)
  
  svmFit <- list()
  for(j in 1:tuneLength)  {
    svmFit[[j]] <- ksvm(formula, data = train, kernel="rbfdot", C=2^(-2 + j), prob.model = T)
  }
  
  svmPredict <- list()
  for(j in 1:tuneLength)  {
    svmPredict[[j]] <- predict(svmFit[[j]], newdata = probeExp, type = "probabilities")[,2]
  }
  
  logiBlend <- list()
  for(j in 1:tuneLength)  {
    logiBlend[[j]] <- blendUpdateClass(svmPredict[[j]], probeRes, P, model)
  }
  
  loss <- vector()
  for(j in 1:tuneLength)  {
    loss[j] <- logiBlend[[j]]$loss
  }
  minIndex <- which.min(loss)
  
  return(list(blendFinal = logiBlend[[minIndex]], modelFinal = svmFit[[minIndex]], predictFinal = svmPredict[[minIndex]]))
}