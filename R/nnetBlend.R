nnetBlendReg <- function (formula, train, probeExp, probeRes, P, model, tuneLength) {
  require(nnet)
  
  #scaling the response variable to be within [0,1]
  formulaObj <- formula(formula, data = train)
  trainResChar <- as.character(formulaObj[[2]])
  trainRes <- train[[trainResChar]]
  minTrainRes <- min(trainRes)
  maxTrainRes <- max(trainRes)
  train[[trainResChar]] <- (1/maxTrainRes)*(trainRes - minTrainRes)
  
  nnetFit <- list()
  for(i in 1:3)  {
    nnetFit[[i]] <- list()
  }
  for(j in 1:tuneLength)  {
    nnetFit[[1]][[j]] <- nnet(formula, data = train, size = 2*j, decay = 0.0001, maxit = 500, trace = F)
    nnetFit[[2]][[j]] <- nnet(formula, data = train, size = 2*j, decay = 0.001, maxit = 500, trace = F)
    nnetFit[[3]][[j]] <- nnet(formula, data = train, size = 2*j, decay = 0.1, maxit = 500, trace = F)
  }
  
  nnetPredictScale <- list()
  for(i in 1:3)  {
    nnetPredictScale[[i]] <- list()
  }
  for(j in 1:tuneLength)  {
    nnetPredictScale[[1]][[j]] <- predict(nnetFit[[1]][[j]], newdata = probeExp)
    nnetPredictScale[[2]][[j]] <- predict(nnetFit[[2]][[j]], newdata = probeExp)
    nnetPredictScale[[3]][[j]] <- predict(nnetFit[[3]][[j]], newdata = probeExp)
  }
  
  #scaling the predictions back to original size
  nnetPredict <- list()
  for(i in 1:3) {
    nnetPredict[[i]] <- list()
  }
  for(j in 1:tuneLength)  {
    nnetPredict[[1]][[j]] <- sapply(nnetPredictScale[[1]][[j]], function(x) maxTrainRes*x + minTrainRes)
    nnetPredict[[2]][[j]] <- sapply(nnetPredictScale[[2]][[j]], function(x) maxTrainRes*x + minTrainRes)
    nnetPredict[[3]][[j]] <- sapply(nnetPredictScale[[3]][[j]], function(x) maxTrainRes*x + minTrainRes)
  }
  
  lmBlend <- list()
  for(i in 1:3)  {
    lmBlend[[i]] <- list()
  }
  for(j in 1:tuneLength)  {
    lmBlend[[1]][[j]] = blendUpdateReg(nnetPredict[[1]][[j]], probeRes, P, model)
    lmBlend[[2]][[j]] = blendUpdateReg(nnetPredict[[2]][[j]], probeRes, P, model)
    lmBlend[[3]][[j]] = blendUpdateReg(nnetPredict[[3]][[j]], probeRes, P, model)
  }
  
  loss <- matrix(nrow = 3, ncol = tuneLength)
  for(i in 1:tuneLength) {
    loss[1,i] <- lmBlend[[1]][[i]]$loss
    loss[2,i] <- lmBlend[[2]][[i]]$loss
    loss[3,i] <- lmBlend[[3]][[i]]$loss
  }
  minIndex <- which(loss == min(loss), arr.ind = T)
  
  #packaging the scaling info to use in the predict.blend function                 
  nnetFit[[minIndex[1,1]]][[minIndex[1,2]]]$max = maxTrainRes
  nnetFit[[minIndex[1,1]]][[minIndex[1,2]]]$min = minTrainRes
  
  return(list(blendFinal = lmBlend[[minIndex[1,1]]][[minIndex[1,2]]], modelFinal = nnetFit[[minIndex[1,1]]][[minIndex[1,2]]], predictFinal = nnetPredict[[minIndex[1,1]]][[minIndex[1,2]]]))
}

#classification
nnetBlendClass <- function (formula, train, probeExp, probeRes, P, model, tuneLength) {
  require(nnet)
  
  #scaling the response variable to be within [0,1]
#  formulaObj <- formula(formula, data = train)
#  trainResChar <- as.character(formulaObj[[2]])
#  trainRes <- train[[trainResChar]]
#  minTrainRes <- min(trainRes)
#  maxTrainRes <- max(trainRes)
#  train[[trainResChar]] <- (1/maxTrainRes)*(trainRes - minTrainRes)
  
  nnetFit <- list()
  for(i in 1:3)  {
    nnetFit[[i]] <- list()
  }
  for(j in 1:tuneLength)  {
    nnetFit[[1]][[j]] <- nnet(formula, data = train, size = 2*j, decay = 0.0001, maxit = 500, trace = F)
    nnetFit[[2]][[j]] <- nnet(formula, data = train, size = 2*j, decay = 0.001, maxit = 500, trace = F)
    nnetFit[[3]][[j]] <- nnet(formula, data = train, size = 2*j, decay = 0.1, maxit = 500, trace = F)
  }
  
  nnetPredictScale <- list()
  for(i in 1:3)  {
    nnetPredictScale[[i]] <- list()
  }
  for(j in 1:tuneLength)  {
    nnetPredictScale[[1]][[j]] <- predict(nnetFit[[1]][[j]], newdata = probeExp)
    nnetPredictScale[[2]][[j]] <- predict(nnetFit[[2]][[j]], newdata = probeExp)
    nnetPredictScale[[3]][[j]] <- predict(nnetFit[[3]][[j]], newdata = probeExp)
  }
  
  #scaling the predictions back to original size
#  nnetPredict <- list()
#  for(i in 1:3) {
#    nnetPredict[[i]] <- list()
#  }
#  for(j in 1:tuneLength)  {
#    nnetPredict[[1]][[j]] <- sapply(nnetPredictScale[[1]][[j]], function(x) maxTrainRes*x + minTrainRes)
#    nnetPredict[[2]][[j]] <- sapply(nnetPredictScale[[2]][[j]], function(x) maxTrainRes*x + minTrainRes)
#    nnetPredict[[3]][[j]] <- sapply(nnetPredictScale[[3]][[j]], function(x) maxTrainRes*x + minTrainRes)
#  }
  
  logiBlend <- list()
  for(i in 1:3)  {
    logiBlend[[i]] <- list()
  }
  for(j in 1:tuneLength)  {
    logiBlend[[1]][[j]] = blendUpdateClass(nnetPredictScale[[1]][[j]], probeRes, P, model)
    logiBlend[[2]][[j]] = blendUpdateClass(nnetPredictScale[[2]][[j]], probeRes, P, model)
    logiBlend[[3]][[j]] = blendUpdateClass(nnetPredictScale[[3]][[j]], probeRes, P, model)
  }
  
  loss <- matrix(nrow = 3, ncol = tuneLength)
  for(i in 1:tuneLength) {
    loss[1,i] <- logiBlend[[1]][[i]]$loss
    loss[2,i] <- logiBlend[[2]][[i]]$loss
    loss[3,i] <- logiBlend[[3]][[i]]$loss
  }
  minIndex <- which(loss == min(loss), arr.ind = T)
  
  #packaging the scaling info to use in the predict.blend function                 
#  nnetFit[[minIndex[1,1]]][[minIndex[1,2]]]$max = maxTrainRes
#  nnetFit[[minIndex[1,1]]][[minIndex[1,2]]]$min = minTrainRes
  
  return(list(blendFinal = logiBlend[[minIndex[1,1]]][[minIndex[1,2]]], modelFinal = nnetFit[[minIndex[1,1]]][[minIndex[1,2]]], predictFinal = nnetPredictScale[[minIndex[1,1]]][[minIndex[1,2]]]))
}