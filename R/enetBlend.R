enetBlendReg <- function (formula, train, probeExp, probeRes, P, model, tuneLength) {
  require(elasticnet)
  
  formulaObj <- formula(formula, data = train)
  trainResChar <- as.character(formulaObj[[2]])
  trainRes <- train[[trainResChar]]
  trainExp <- as.matrix(subset(train, select = -get(trainResChar)))
  
  enetFit <- list()
  for(i in 1:(tuneLength+1))  {
  enetFit[[i]] <- enet(trainExp, trainRes, lambda = ((i-1)*(1 / tuneLength)))
  }
                  
  enetPredict <- list()
  for(i in 1:(tuneLength+1))  {
  enetPredict[[i]] <- predict(enetFit[[i]], newx = probeExp, type = "fit")  #this is a matrix
  }
  
  lmBlend <- list()
  for(i in 1:(tuneLength+1)) {
    lmBlend[[i]] <- list()
    for(j in 2:(length(enetFit[[1]]$Cp)))  {
      lmBlend[[i]][[j]] = blendUpdateReg(enetPredict[[i]]$fit[,j], probeRes, P, model)
    }  
  }
  
  loss <- matrix(nrow = (tuneLength + 1), ncol = (length(enetFit[[1]]$Cp) - 1))
  for(i in 1:(tuneLength+1)) {
    for(j in 2:(length(enetFit[[1]]$Cp)))  {
    loss[i,j - 1] <- lmBlend[[i]][[j]]$loss
    }
  }
  
  minIndex <- matrix(nrow = 3, ncol = 2 )
  for(i in 1:3) {
    minIndex[i,1] = which.min(loss[i,])
    minIndex[i,2] = loss[i, minIndex[i,1]]
  }
  model <- which.min(minIndex[,2]) 
  step <- minIndex[model, 1] + 1
  
  enetFit[[model]]$step <- step  #need to send which step info to the predict function
  
  return(list(blendFinal = lmBlend[[model]][[step]], modelFinal = enetFit[[model]], predictFinal = enetPredict[[model]]$fit[,step]))
}