\name{scorePreprocessor.notes}
\alias{scorePreprocessor.notes}
\title{Score preprocessing for shape 'notes'}
\usage{
  \method{scorePreprocessor}{notes} (sonlayerscore)
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
  This function does some needed preprocessing for shape
  notes to get the method ready to render. In this case, it
  takes the 'time' and 'tempo' mappings and translates them
  into start times. It also scales duration accordingly.
}
\keyword{internal}

