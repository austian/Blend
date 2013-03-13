#main function of the linear blending package
#blendSwitch, blendUpdate, and the various "model"Blend functions are used for a blend call
#inputs are formula,
#           training dataframe,           
#           probe dataframe, 
#           and a character vector of model names
#           a number tuneLength that selects the number of tuning parameters to consider for each model
#output is a list representing the blended linear model and the individual models used in the blend

blendAlgo <- function (formula, train, probe, models, tuneLength = 3)  {
  
  #splitting the probe df into explanatory and response parts
  formulaObj <- formula(formula, data = train)
  probeResChar <- as.character(formulaObj[[2]])
  probeRes <- probe[[probeResChar]]
  probeExp <- subset(probe, select = -get(probeResChar))
  
  #running the blending algorithm for each model in the list, see blendSwitch.R
  #different calls to the individual models and the blendUpdate functions are required for classification or regression
  bool <- (class(probeRes) == "numeric")
  if(bool){
    
    #initializing the first stage of the blending algorithm with a constant model  
    P <- matrix(1, length(probeRes), 1)
    colnames(P) <- c("int")
    
    returnBlend <- list()
    
    for(i in 1:length(models)){
      currentBlend <- blendSwitchReg(models[[i]], formula, train, probeExp, probeRes, P, tuneLength)
    
      P <- cbind(P, currentBlend$predictFinal)
      colnames(P)[i+1] <- models[[i]]
    
      returnBlend[["blend"]] <- currentBlend$blendFinal$blend
      returnBlend[[models[[i]]]] <- currentBlend$modelFinal
    }
  
    class(returnBlend) <- "blend"
    return(returnBlend)
  }
  else{
    
    #initializing the first stage of the blending algorithm with a constant model
    P <- matrix(1, length(probeRes), 1)
    colnames(P) <- c("int")
 
    returnBlend <- list()
    returnBlend[["levels"]] <- levels(probeRes)
    
    for(i in 1:length(models)){
      currentBlend <- blendSwitchClass(models[[i]], formula, train, probeExp, probeRes, P, tuneLength)
      
      P <- cbind(P, currentBlend$predictFinal)
      colnames(P)[i+1] <- models[[i]]
      
      returnBlend[["blend"]] <- currentBlend$blendFinal$blend
      returnBlend[[models[[i]]]] <- currentBlend$modelFinal
    }
    class(returnBlend) <- "blend"
    return(returnBlend)
  }        
}                                     
                                         