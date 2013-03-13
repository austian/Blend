\name{mpgData}
\alias{mpgData}
\docType{data}
\title{
mpgData
}
\description{
Slightly modified version of a data set from the UCI repository relating to car fuel efficiency from the 70's.
}
\usage{data(mpgData)}
\format{
  A data frame with 385 observations on the following 8 variables.
  \describe{
    \item{\code{mpg}}{a numeric vector}
    \item{\code{cyl}}{a numeric vector}
    \item{\code{disp}}{a numeric vector}
    \item{\code{horse}}{a numeric vector}
    \item{\code{wgt}}{a numeric vector}
    \item{\code{acc}}{a numeric vector}
    \item{\code{yr}}{a numeric vector}
    \item{\code{origin}}{a numeric vector}
  }
}
\details{
Some of the entries were missing a horsepower record so they were dropped.  There also were some 3 and 5 cylinder car records and they were dropped too.  Finally a column consisting of the names of the cars was dropped also.
}
\source{
http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(mpgData)
## maybe str(mpgData) ; plot(mpgData) ...
}
\keyword{datasets}
