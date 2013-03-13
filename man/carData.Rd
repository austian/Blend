\name{carData}
\alias{carData}
\docType{data}
\title{
carData
}
\description{
A slightly modified data set from the UCI repository pretaining to car recommendations.
}
\usage{data(carData)}
\format{
  A data frame with 1728 observations on the following 7 variables.
  \describe{
    \item{\code{V1}}{a factor with levels \code{high} \code{low} \code{med} \code{vhigh}}
    \item{\code{V2}}{a factor with levels \code{high} \code{low} \code{med} \code{vhigh}}
    \item{\code{V3}}{a factor with levels \code{2} \code{3} \code{4} \code{5more}}
    \item{\code{V4}}{a factor with levels \code{2} \code{4} \code{more}}
    \item{\code{V5}}{a factor with levels \code{big} \code{med} \code{small}}
    \item{\code{V6}}{a factor with levels \code{high} \code{low} \code{med}}
    \item{\code{V7}}{a factor with levels \code{acc} \code{unacc}}
  }
}
\details{
The response variable V7 was originally four levels, they were combined into two.
}
\source{
http://archive.ics.uci.edu/ml/machine-learning-databases/car/
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(carData)
## maybe str(carData) ; plot(carData) ...
}
\keyword{datasets}
