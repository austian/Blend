earthBlendReg <- function (formula, train, probeExp, probeRes, P, model, tuneLength) {
  require(earth)
  
  earthFit <-list()
  for(i in 1:3)  {
    earthFit[[i]] <- list()
  }
  for(j in 1:tuneLength)  {
    earthFit[[1]][[j]] <- earth(formula, data = train, nprune = j+1, degree = 1) 
    earthFit[[2]][[j]] <- earth(formula, data = train, nprune = j+1, degree = 2)
    earthFit[[3]][[j]] <- earth(formula, data = train, nprune = j+1, degree = 3)
  }
  
  earthPredict <- list()
  for(i in 1:3)  {
    earthPredict[[i]] <- list()
  }
  for(j in 1:tuneLength)  {
    earthPredict[[1]][[j]] <- predict(earthFit[[1]][[j]], newdata = probeExp)
    earthPredict[[2]][[j]] <- predict(earthFit[[2]][[j]], newdata = probeExp)
    earthPredict[[3]][[j]] <- predict(earthFit[[3]][[j]], newdata = probeExp)
  }
  
  lmBlend <- list()
  for(i in 1:3)  {
    lmBlend[[i]] <- list()
  }
  for(j in 1:tuneLength)  {
    lmBlend[[1]][[j]] <- blendUpdateReg(earthPredict[[1]][[j]], probeRes, P, model)
    lmBlend[[2]][[j]] <- blendUpdateReg(earthPredict[[2]][[j]], probeRes, P, model)
    lmBlend[[3]][[j]] <- blendUpdateReg(earthPredict[[3]][[j]], probeRes, P, model)
  }
  
  loss <- matrix(nrow = 3, ncol = tuneLength)
  for(i in 1:tuneLength) {
    loss[1,i] <- lmBlend[[1]][[i]]$loss
    loss[2,i] <- lmBlend[[2]][[i]]$loss
    loss[3,i] <- lmBlend[[3]][[i]]$loss
  }
  minIndex <- which(loss == min(loss), arr.ind = T)
  
  return(list(blendFinal = lmBlend[[minIndex[1]]][[minIndex[2]]], modelFinal = earthFit[[minIndex[1]]][[minIndex[2]]], predictFinal = earthPredict[[minIndex[1]]][[minIndex[2]]]))
}

earthBlendClass <- function (formula, train, probeExp, probeRes, P, model, tuneLength) {
  require(earth)
  
  earthFit <-list()
  for(i in 1:3)  {
    earthFit[[i]] <- list()
  }
  for(j in 1:tuneLength)  {
    earthFit[[1]][[j]] <- earth(formula, data = train, nprune = j+1, degree = 1, glm=list(family=binomial)) 
    earthFit[[2]][[j]] <- earth(formula, data = train, nprune = j+1, degree = 2, glm=list(family=binomial))
    earthFit[[3]][[j]] <- earth(formula, data = train, nprune = j+1, degree = 3, glm=list(family=binomial))
  }
  
  earthPredict <- list()
  for(i in 1:3)  {
    earthPredict[[i]] <- list()
  }
  for(j in 1:tuneLength)  {
    earthPredict[[1]][[j]] <- predict(earthFit[[1]][[j]], newdata = probeExp, type = "response")
    earthPredict[[2]][[j]] <- predict(earthFit[[2]][[j]], newdata = probeExp, type = "response")
    earthPredict[[3]][[j]] <- predict(earthFit[[3]][[j]], newdata = probeExp, type = "response")
  }
  
  logiBlend <- list()
  for(i in 1:3)  {
    logiBlend[[i]] <- list()
  }
  for(j in 1:tuneLength)  {
    logiBlend[[1]][[j]] <- blendUpdateClass(earthPredict[[1]][[j]], probeRes, P, model)
    logiBlend[[2]][[j]] <- blendUpdateClass(earthPredict[[2]][[j]], probeRes, P, model)
    logiBlend[[3]][[j]] <- blendUpdateClass(earthPredict[[3]][[j]], probeRes, P, model)
  }
  
  loss <- matrix(nrow = 3, ncol = tuneLength)
  for(i in 1:tuneLength) {
    loss[1,i] <- logiBlend[[1]][[i]]$loss
    loss[2,i] <- logiBlend[[2]][[i]]$loss
    loss[3,i] <- logiBlend[[3]][[i]]$loss
  }
  minIndex <- which(loss == min(loss), arr.ind = T)
  
  return(list(blendFinal = logiBlend[[minIndex[1]]][[minIndex[2]]], modelFinal = earthFit[[minIndex[1]]][[minIndex[2]]], predictFinal = earthPredict[[minIndex[1]]][[minIndex[2]]]))
}