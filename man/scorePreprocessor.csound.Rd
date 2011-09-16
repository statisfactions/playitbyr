\name{scorePreprocessor.csound}
\alias{scorePreprocessor.csound}
\title{Score preprocessing for shape 'csound'}
\usage{
  \method{scorePreprocessor}{csound} (sonlayerscore)
}
\arguments{
  \item{sonlayerscore}{The score generated for a specific
  \code{sonlayer} by \code{.getSonlayerScore()}}
}
\value{
  A sonlayer score with the transformations described in
  the 'Description' field
}
\description{
  This does preprocessing for shape 'csound'. All this does
  is return the score with attribute 'length' added.
}
\keyword{internal}

