\name{predict.blend}
\alias{predict.blend}
\title{Prediction function for blend objects}
\usage{
  predict.blend(blendObj, testX)
}
\arguments{
  \item{blendObj}{the fitted blend object}

  \item{testX}{the dataframe of new explanatory (i.e. "X")
  variables to predict on, must exactly match the structure
  of the data frame used to fit the blend object}
}
\value{
  a numeric vector in the regression case, a dataframe in
  the classification case
}
\description{
  Predicts on new data using a blend object
}
\details{
  For regression a numeric vector of predicted values is
  outputted.  For classification a dataframe is outputted,
  with first column the predicted probabilities and second
  column the predicted classes.
}
\examples{
data(mpgData)
modelVec <- c("gbm", "svm", "lm")
blendReg <- blend(mpg~., data = mpgData, models = modelVec, tuneLength = 5, folds = 10, repeats = 3)
predict(blendReg, mpgData)

data(carData)
modelVec <- c("gbm", "svm", "logi")
blendClass <- blend(V7~., data = carData, models = modelVec, tuneLength = 5, folds = 5, repeats = 2)
predict(blendClass, carData)
}
\author{
  Alex Ustian <alex.l.ustian@gmail.com>
}

