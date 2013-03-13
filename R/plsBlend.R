plsBlendReg <- function (formula, train, probeExp, probeRes, P, model) {
  require(pls)
  
  plsrFit <- plsr(formula, data = train, scale = T)
  plsrPredict <- predict(plsrFit, newdata = probeExp)  #this is a matrix
  
  lmBlend <- list()
  for(i in 1:plsrFit$ncomp) {
    lmBlend[[i]] <- blendUpdateReg(plsrPredict[,1,i], probeRes, P, model)
  }
  
  loss <- vector()
  for(i in 1:plsrFit$ncomp) {
    loss[i] <- lmBlend[[i]]$loss
  }
  
  minIndex <- which.min(loss)
  
  plsrFit$comp <- minIndex  #need to send how many components to the predict function
  
  return(list(blendFinal = lmBlend[[minIndex]], modelFinal = plsrFit, predictFinal = plsrPredict[,1,minIndex]))
}

plsBlendClass <- function (formula, train, probeExp, probeRes, P, model) {
  require(pls)
  
  plsrFit <- plsr(formula, data = train, scale = T)
  plsrPredict <- predict(plsrFit, newdata = probeExp)  #this is a matrix
  
  logiBlend <- list()
  for(i in 1:plsrFit$ncomp) {
    logiBlend[[i]] <- blendUpdateClass(plsrPredict[,1,i], probeRes, P, model)
  }
  
  loss <- vector()
  for(i in 1:plsrFit$ncomp) {
    loss[i] <- logiBlend[[i]]$loss
  }
  
  minIndex <- which.min(loss)
  
  plsrFit$comp <- minIndex  #need to send how many components to the predict function
  
  return(list(blendFinal = logiBlend[[minIndex]], modelFinal = plsrFit, predictFinal = plsrPredict[,1,minIndex]))
}