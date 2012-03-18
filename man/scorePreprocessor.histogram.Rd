\name{scorePreprocessor.histogram}
\alias{scorePreprocessor.histogram}
\title{Score preprocessing for shape 'histogram'}
\usage{
  \method{scorePreprocessor}{histogram} (sonlayerscore,
    opts, ...)
}
\arguments{
  \item{sonlayerscore}{The score generated for a specific
  \code{sonlayer} by \code{.getSonlayerScore()}}

  \item{opts}{The options passed as shape parameters in a
  \code{sonlayer}}
}
\value{
  A sonlayer score with the transformations described in
  the 'Description' field
}
\description{
  This function does some needed preprocessing for shape
  histogram to get the method ready to render. In this
  case, it takes the 'time' and 'tempo' mappings and
  translates them into start times. It also scales duration
  accordingly.
}
\keyword{internal}

