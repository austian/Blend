logiBlend <- function (formula, train, probeExp, probeRes, P, model, tuneLength) {
  logiFit <- glm(formula, data = train, family = "binomial")
  logiPredict <- predict(logiFit, newdata = probeExp, type = "response")
  logilogiBlend <- blendUpdateClass(logiPredict, probeRes, P, model)                  
  loss <- logilogiBlend$loss
  return(list(blendFinal = logilogiBlend, modelFinal = logiFit, predictFinal = logiPredict))
}