ctreeBlendReg <- function (formula, train, probeExp, probeRes, P, model, tuneLength) {
  require(party)
  
  ctreeFit <- list()
  for(i in 1:tuneLength+1)  {
    ctreeFit[[i]] <- ctree(formula, data = train, controls = ctree_control(mincriterion = (.01 + ((.99 - .01)/tuneLength)*(i-1))))
  }
  
  ctreePredict <- list()
  for(i in 1:tuneLength+1)  {
    ctreePredict[[i]] <- predict(ctreeFit[[i]], newdata = probeExp)
  }
  
  lmBlend <- list()
  for(i in 1:tuneLength+1)  {
    lmBlend[[i]] <- blendUpdateReg(ctreePredict[[i]], probeRes, P, model)
  }
  
  loss <- vector()
  for(i in 1:tuneLength+1)  {
    loss[i] <- lmBlend[[i]]$loss
  }
  
  minIndex <- which.min(loss)
  
  return(list(blendFinal = lmBlend[[minIndex]], modelFinal = ctreeFit[[minIndex]], predictFinal = ctreePredict[[minIndex]]))
}

ctreeBlendClass <- function (formula, train, probeExp, probeRes, P, model, tuneLength) {
  require(party)
  
  ctreeFit <- list()
  for(i in 1:tuneLength+1)  {
    ctreeFit[[i]] <- ctree(formula, data = train, controls = ctree_control(mincriterion = (.01 + ((.99 - .01)/tuneLength)*(i-1))))
  }
  
  ctreePredict <- list()
  for(i in 1:tuneLength+1)  {
    ctreePredict[[i]] <- sapply(predict(ctreeFit[[i]], newdata = probeExp, type = "prob"), function(x) x[2])
  }
  
  logiBlend <- list()
  for(i in 1:tuneLength+1)  {
    logiBlend[[i]] <- blendUpdateClass(ctreePredict[[i]], probeRes, P, model)
  }
  
  loss <- vector()
  for(i in 1:tuneLength+1)  {
    loss[i] <- logiBlend[[i]]$loss
  }
  
  minIndex <- which.min(loss)
  
  return(list(blendFinal = logiBlend[[minIndex]], modelFinal = ctreeFit[[minIndex]], predictFinal = ctreePredict[[minIndex]]))
}