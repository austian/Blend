lmBlend <- function (formula, train, probeExp, probeRes, P, model) {
  lmFit <- lm(formula, data = train)
  lmPredict <- predict(lmFit, newdata = probeExp)
  lmlmBlend <- blendUpdateReg(lmPredict, probeRes, P, model)                  
  loss <- lmlmBlend$loss
  return(list(blendFinal = lmlmBlend, modelFinal = lmFit, predictFinal = lmPredict))
}