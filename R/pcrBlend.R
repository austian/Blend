pcrBlendReg <- function (formula, train, probeExp, probeRes, P, model) {
  require(pls)
  
  pcrFit <- pcr(formula, data = train, scale = T)
  pcrPredict <- predict(pcrFit, newdata = probeExp)  #this is a matrix
  
  lmBlend <- list()
  for(i in 1:pcrFit$ncomp) {
    lmBlend[[i]] <- blendUpdateReg(pcrPredict[,1,i], probeRes, P, model)
  }
  
  loss <- vector()
  for(i in 1:pcrFit$ncomp) {
    loss[i] <- lmBlend[[i]]$loss
  }
  
  minIndex <- which.min(loss)
  
  pcrFit$comp <- minIndex  #need to send how many components to the predict function
  
  return(list(blendFinal = lmBlend[[minIndex]], modelFinal = pcrFit, predictFinal = pcrPredict[,1,minIndex]))
}