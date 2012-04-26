\name{scorePreprocessor}
\alias{scorePreprocessor}
\title{Generic method for preprocessing score layers}
\usage{
  scorePreprocessor(sonlayerscore, opts, ...)
}
\arguments{
  \item{sonlayerscore}{The score generated for a specific
  \code{sonlayer} by \code{.getSonlayerScore()}}

  \item{opts}{The options passed as shape parameters in a
  \code{sonlayer}}

  \item{\dots}{Other things to pass to scorePreprocessor.
  Currently ignored.}
}
\value{
  A score after shape-specific processing has been
  completed.
}
\description{
  Generic to do extra needed processing and calulation for
  a specific sonlayer shape.
}
\note{
  all scorePreprocessor methods must calculate length of
  sonification and return that as an attribute
  \code{length} of the data frame.
}
\keyword{internal}

