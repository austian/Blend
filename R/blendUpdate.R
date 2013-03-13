#used inside the blend function
#this function carries out the actual linear/logistic model blending of the current model and the previous models' predictions
#inputs are a vector of the model's predictions on the probe set, 
#           a vector of the probe set's response variable,
#           a matrix P of the previous model's probe predictions
#           a character type containing the model's name
#output is the linear/logistic model representing the current blend and a number that is loss of the current blended model


blendUpdateReg <- function (predict, probeRes, P, model) {
  X <- cbind(P, predict)
  colnames(X)[dim(X)[2]] <- model
  probeDf <- cbind.data.frame(probeRes, X)
  lmBlend <- lm(probeRes ~ . - 1, data = probeDf)
  p <- predict(lmBlend, newdata = probeDf)
  return(list(blend = lmBlend, loss = loss(p, probeRes)))
}

blendUpdateClass <- function (predict, probeRes, P, model) {
  X <- cbind.data.frame(P, predict)
  colnames(X)[dim(X)[2]] <- model
  probeDf <- cbind.data.frame(probeRes, X)
  logiBlend <- glm(probeRes ~ . - 1, data = probeDf, family = binomial)
  p <- factor(ifelse(predict(logiBlend, newdata = probeDf, type = "response") < .5, levels(probeRes)[1], levels(probeRes)[2]))
  levels(p) <- levels(probeRes)
  return(list(blend = logiBlend, loss = loss(p, probeRes)))
}