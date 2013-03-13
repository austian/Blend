\name{blend}
\alias{blend}
\title{Linear model-blending function}
\usage{
  blend(formula, data = NULL, train = NULL, probe = NULL,
    models = "lm", tuneLength = 3, folds = 1, repeats = 1,
    p = 0.7)
}
\arguments{
  \item{formula}{standard R formula as used in \code{lm}}

  \item{data}{data set to be used}

  \item{train}{if the user wants to manually split the data
  set into training and probe sets, this argument is for
  the training set, otherwise leave NULL}

  \item{probe}{for manual splitting, this is the probe set,
  otherwise leave NULL}

  \item{models}{a character vector of supported models to
  use in the blend}

  \item{tuneLength}{the number of tuning parameters to
  consider when blending the individual models}

  \item{folds}{the number of folds to use for
  cross-validation}

  \item{repeats}{the number of repetitions to use for
  cross-validation}

  \item{p}{the percentage of the data set to be used as the
  training set}
}
\value{
  a blend object
}
\description{
  Uses linear or logistic regression to blend multiple
  models, forming a new predictive model.  Linear
  regression for numeric responses and logistic regression
  for two-level factor responses.
}
\details{
  This is the primary function of the package.  Loosely
  based on the Netflix Grand Prize winning algorithm (see
  the references for the full official discussion by one of
  the winning teams BigChaos, Andreas Toescher and Michael
  Jahrerfor, and a pedestrian discussion by Alex Ustian),
  this function takes multiple model types and performs a
  linear(logistic) regression to find the optimal
  coefficients to use in a linear(logistic) combination of
  the input models.

  For this algorithm, a data set is split up into two
  pieces.  A set used to train the individual models, which
  we refer to as the training set, and a set used to fit
  the linear/logistic blend of the models called the probe
  set.  Note that there is some overlap of terminology.
  Normally one splits a data set into two parts, a training
  and test set.  In terms of a test set, in this analysis
  one is splitting up a data set into three parts, a test
  set, a probe set, and a training set.

  Currently supported models:

  Regression (lm, rpart, ctree, svm, pls, pcr, enet, rf,
  earth, nnet, gbm)

  Classification (logi, rpart, ctree, svm, rf, earth, nnet,
  gbm)
}
\examples{
data(mpgData)
modelVec <- c("gbm", "svm", "lm")
blendReg <- blend(mpg~., data = mpgData, models = modelVec, tuneLength = 5, folds = 10, repeats = 3)

data(carData)
modelVec <- c("gbm", "svm", "logi")
blendClass <- blend(V7~., data = carData, models = modelVec, tuneLength = 5, folds = 5, repeats = 2)
}
\author{
  Alex Ustian <alex.l.ustian@gmail.com>

  Special Note: None of the winning Netflix teams are
  associated with this project, please direct all ire
  towards Alex Ustian.
}
\references{
  The BigChaos Solution to the Netflix Grand Prize, Andreas
  Toescher and Michael Jahrer
  \link{http://www.netflixprize.com/assets/GrandPrize2009_BPC_BigChaos.pdf}

  Linear model-blending, Alex Ustian \link{http://}
}

