rpartBlendReg <- function (formula, train, probeExp, probeRes, P, model, tuneLength) {
  require(rpart)
  
  rpartFit <- list()
  for(i in 1:tuneLength)  {
    rpartFit[[i]] <- rpart(formula, data = train, cp = 0.005*i, method = "anova")
  }
  
  rpartPredict <- list()
  for(i in 1:tuneLength)  {
    rpartPredict[[i]] <- predict(rpartFit[[i]], newdata = probeExp)
  }
  
  lmBlend <- list()
  for(i in 1:tuneLength)  {
    lmBlend[[i]] <- blendUpdateReg(rpartPredict[[i]], probeRes, P, model)
  }
  
  loss <- vector()
  for(i in 1:tuneLength)  {
    loss[i] <- lmBlend[[i]]$loss
  }

  minIndex <- which.min(loss)
  
  return(list(blendFinal = lmBlend[[minIndex]], modelFinal = rpartFit[[minIndex]], predictFinal = rpartPredict[[minIndex]]))
}

rpartBlendClass <- function (formula, train, probeExp, probeRes, P, model, tuneLength) {
  require(rpart)
  
  rpartFit <- list()
  for(i in 1:tuneLength)  {
    rpartFit[[i]] <- rpart(formula, data = train, cp = 0.005*i, method = "class")
  }
  
  rpartPredict <- list()
  for(i in 1:tuneLength)  {
    rpartPredict[[i]] <- predict(rpartFit[[i]], newdata = probeExp, type = "prob")[,2]
  }
  
  logiBlend <- list()
  for(i in 1:tuneLength)  {
    logiBlend[[i]] <- blendUpdateClass(rpartPredict[[i]], probeRes, P, model)
  }
  
  loss <- vector()
  for(i in 1:tuneLength)  {
    loss[i] <- logiBlend[[i]]$loss
  }
  minIndex <- which.min(loss)
  
  return(list(blendFinal = logiBlend[[minIndex]], modelFinal = rpartFit[[minIndex]], predictFinal = rpartPredict[[minIndex]]))
}