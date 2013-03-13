#cross validation support functions

foldGenerator <- function(rowdim, folds)  {
  
  foldSize <- as.integer(rowdim / folds)
  dimVec <- 1:rowdim
  
  #creating fold categories to split by
  foldCats <- vector()
  leftover <- rowdim %% folds
  if(leftover >= 1)  {
    for(i in 1:leftover)  {
      foldCats <- c(foldCats, rep(i, foldSize + 1))
    }
  }
  for(i in (leftover + 1):folds)  {
    foldCats <- c(foldCats, rep(i, foldSize))
  }
  
  #splitting the data row indices into equal groups
  foldSplit <- split(dimVec, sample(foldCats))
  
  #creating the output list
  foldList <- list()
  for(i in 1:folds)  {
    foldList[[i]] <- list()
    foldVec <- vector()
    foldTrain <- foldSplit[names(foldSplit) != i]
    for(j in 1:(folds - 1))  {
      foldVec <- c(foldVec, foldTrain[[j]])
    }
    foldList[[i]][[1]] <- foldVec
    foldList[[i]][[2]] <- foldSplit[[i]]
  }
  return(foldList) 
}

trainGenerator <- function(foldSeq, p = 0.7)  {
  return(sample(foldSeq, as.integer(p*length(foldSeq)))) 
}

probeGenerator <- function(foldSeq, trainSeq)  {
  probeBool <- !(foldSeq %in% trainSeq)
  return(foldSeq[probeBool])
}

error <- function(formula, model, newdata)  {
  
  #splitting the newdata df into explanatory and response parts
  formulaObj <- formula(formula, data = newdata)
  newdataResChar <- as.character(formulaObj[[2]])
  newdataRes <- newdata[[newdataResChar]]
  newdataExp <- subset(newdata, select = -get(newdataResChar))
  
  if(class(newdataRes) == "numeric")  {
    blendPredict <- predict(model, newdataExp)
  }
  else  {
    blendPredict <- predict(model, newdataExp)[,2]
  }
  return(loss(blendPredict, newdataRes))
}