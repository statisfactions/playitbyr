\name{.getScore}
\alias{.getScore}
\title{Internal functions to generate output \code{data.frame} from a
\code{sonify} object.}
\usage{
  .getScore(x)
}
\arguments{
  \item{x}{A \code{sonify} object}
}
\value{
  The end product of all this is a list of
  \code{data.frame} objects that is used as an input to the
  rendering process
}
\description{
  These functions, together, are used to generate a
  standardized \code{data.frame}, rather like a Csound
  score, that is then used by whatever method renders the
  sounds (currently only rendering in R as sine waves is
  supported).
}
\note{
  These functions (particularly .SonlayerScore) assume that
  every shape has a single output row for each input data
  row after applying statistical tranformations
}
\keyword{internal}

