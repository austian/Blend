#regression
gbmBlendReg <- function (formula, train, probeExp, probeRes, P, model, tuneLength) {
  require(gbm)
  
  gbmFit <- list()
  for(i in 1:3) {
    gbmFit[[i]] <- list()
  }
  for(i in 1:tuneLength) {
    gbmFit[[1]][[i]] <- gbm(formula, data = train, distribution = "gaussian", interaction.depth = 1, n.trees = 100*i, verbose = F) 
    gbmFit[[2]][[i]] <- gbm(formula, data = train, distribution = "gaussian", interaction.depth = 2, n.trees = 100*i, verbose = F)
    gbmFit[[3]][[i]] <- gbm(formula, data = train, distribution = "gaussian", interaction.depth = 3, n.trees = 100*i, verbose = F)
  }
  
  gbmPredict <- list()
  for(i in 1:3) {
    gbmPredict[[i]] <- list()
  }
  for(i in 1:tuneLength) {
    gbmPredict[[1]][[i]] <- predict(gbmFit[[1]][[i]], newdata = probeExp, n.trees = 100*i)
    gbmPredict[[2]][[i]] <- predict(gbmFit[[2]][[i]], newdata = probeExp, n.trees = 100*i)
    gbmPredict[[3]][[i]] <- predict(gbmFit[[3]][[i]], newdata = probeExp, n.trees = 100*i)
  }
  
  lmBlend <- list()
  for(i in 1:3) {
    lmBlend[[i]] <- list()
  }
  for(i in 1:tuneLength) {
    lmBlend[[1]][[i]] <- blendUpdateReg(gbmPredict[[1]][[i]], probeRes, P, model)
    lmBlend[[2]][[i]] <- blendUpdateReg(gbmPredict[[2]][[i]], probeRes, P, model) 
    lmBlend[[3]][[i]] <- blendUpdateReg(gbmPredict[[3]][[i]], probeRes, P, model)
  }
  
  loss <- matrix(nrow = 3, ncol = tuneLength)
  for(i in 1:tuneLength) {
    loss[1,i] <- lmBlend[[1]][[i]]$loss
    loss[2,i] <- lmBlend[[2]][[i]]$loss
    loss[3,i] <- lmBlend[[3]][[i]]$loss
  }
  minIndex <- which(loss == min(loss), arr.ind = T)
  
  return(list(blendFinal = lmBlend[[minIndex[1]]][[minIndex[2]]], modelFinal = gbmFit[[minIndex[1]]][[minIndex[2]]], predictFinal = gbmPredict[[minIndex[1]]][[minIndex[2]]]))
}

#classification
gbmBlendClass <- function (formula, train, probeExp, probeRes, P, model, tuneLength) {
  require(gbm)
  
  #gbm requires the response variable to be numeric and to be in {0,1}
  formulaObj <- formula(formula, data = train)
  trainResChar <- as.character(formulaObj[[2]])
  train[[trainResChar]] <- as.numeric(train[[trainResChar]])
  train[[trainResChar]] <- sapply(train[[trainResChar]], function(x) {if(x == 1){x = 0}else{x = 1}})
  
  gbmFit <- list()
  for(i in 1:3) {
    gbmFit[[i]] <- list()
  }
  for(i in 1:tuneLength) {
    gbmFit[[1]][[i]] <- gbm(formula, data = train, distribution = "bernoulli", interaction.depth = 1, n.trees = 100*i, verbose = F) 
    gbmFit[[2]][[i]] <- gbm(formula, data = train, distribution = "bernoulli", interaction.depth = 2, n.trees = 100*i, verbose = F)
    gbmFit[[3]][[i]] <- gbm(formula, data = train, distribution = "bernoulli", interaction.depth = 3, n.trees = 100*i, verbose = F)
  }
  
  gbmPredict <- list()
  for(i in 1:3) {
    gbmPredict[[i]] <- list()
  }
  for(i in 1:tuneLength) {
    gbmPredict[[1]][[i]] <- predict(gbmFit[[1]][[i]], newdata = probeExp, n.trees = 100*i, type = "response") 
    gbmPredict[[2]][[i]] <- predict(gbmFit[[2]][[i]], newdata = probeExp, n.trees = 100*i, type = "response") 
    gbmPredict[[3]][[i]] <- predict(gbmFit[[3]][[i]], newdata = probeExp, n.trees = 100*i, type = "response")
  }
  
  logiBlend <- list()
  for(i in 1:3) {
    logiBlend[[i]] <- list()
  }
  for(i in 1:tuneLength) {
    logiBlend[[1]][[i]] <- blendUpdateClass(gbmPredict[[1]][[i]], probeRes, P, model)
    logiBlend[[2]][[i]] <- blendUpdateClass(gbmPredict[[2]][[i]], probeRes, P, model) 
    logiBlend[[3]][[i]] <- blendUpdateClass(gbmPredict[[3]][[i]], probeRes, P, model)
  }
  
  loss <- matrix(nrow = 3, ncol = tuneLength)
  for(i in 1:tuneLength) {
    loss[1,i] <- logiBlend[[1]][[i]]$loss
    loss[2,i] <- logiBlend[[2]][[i]]$loss
    loss[3,i] <- logiBlend[[3]][[i]]$loss
  }
  minIndex <- which(loss == min(loss), arr.ind = T)
  
  return(list(blendFinal = logiBlend[[minIndex[1]]][[minIndex[2]]], modelFinal = gbmFit[[minIndex[1]]][[minIndex[2]]], predictFinal = gbmPredict[[minIndex[1]]][[minIndex[2]]]))
}
