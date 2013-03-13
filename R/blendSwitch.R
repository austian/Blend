#used inside of blend
#support function that switches between individual model types to blend
#view each model's blend function for the actual implementation of the algorithm

blendSwitchReg <- function(model, formula, train, probeExp, probeRes, P, tuneLength)  {
  
  switch(model, 
         rpart = rpartBlendReg(formula, train, probeExp, probeRes, P, model, tuneLength), 
         lm = lmBlend(formula, train,probeExp, probeRes, P, model),
         ctree = ctreeBlendReg(formula, train, probeExp, probeRes, P, model, tuneLength),
         nnet = nnetBlendReg(formula, train, probeExp, probeRes, P, model, tuneLength),
         gbm = gbmBlendReg(formula, train, probeExp, probeRes, P, model, tuneLength),
         enet = enetBlendReg(formula, train, probeExp, probeRes, P, model, tuneLength),
         pls = plsBlendReg(formula, train, probeExp, probeRes, P, model),
         pcr = pcrBlendReg(formula, train, probeExp, probeRes, P, model),
         earth = earthBlendReg(formula, train, probeExp, probeRes, P, model, tuneLength),
         rf = rfBlendReg(formula, train, probeExp, probeRes, P, model, tuneLength),
         svm = svmBlendReg(formula, train, probeExp, probeRes, P, model, tuneLength))
}

blendSwitchClass <- function(model, formula, train, probeExp, probeRes, P, tuneLength)  {
  
  switch(model, 
         rpart = rpartBlendClass(formula, train, probeExp, probeRes, P, model, tuneLength), 
         logi = logiBlend(formula, train, probeExp, probeRes, P, model, tuneLength),
         ctree = ctreeBlendClass(formula, train, probeExp, probeRes, P, model, tuneLength),
         nnet = nnetBlendClass(formula, train, probeExp, probeRes, P, model, tuneLength),
         gbm = gbmBlendClass(formula, train, probeExp, probeRes, P, model, tuneLength),
         pls = plsBlendClass(formula, train, probeExp, probeRes, P, model),
         earth = earthBlendClass(formula, train, probeExp, probeRes, P, model, tuneLength),
         rf = rfBlendClass(formula, train, probeExp, probeRes, P, model, tuneLength),
         svm = svmBlendClass(formula, train, probeExp, probeRes, P, model, tuneLength))
}