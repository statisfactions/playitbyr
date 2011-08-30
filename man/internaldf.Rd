\name{internaldf}
\alias{.getScore}
\alias{.getSonlayerData}
\alias{.getSonlayerMappings}
\alias{.getSonlayerScore}
\alias{internaldf}
\title{Internal functions to generate output \code{data.frame} from a
\code{sonify} object.}
\usage{
  .getScore(s)

  .getSonlayerScore(x, sonlayernum)

  .getSonlayerMappings(x, sonlayernum)

  .getSonlayerData(x, sonlayernum)
}
\arguments{
  \item{s,x}{A \code{sonify} object}

  \item{sonlayernum}{The layer number of the \code{sonify}
  object. This is a placeholder for future implementation
  of a layering functionality (modeled after \code{ggplot2}
  objects).}
}
\value{
  The end product of all this is a \code{data.frame} object
  that is used as an input to the rendering process
}
\description{
  These functions, together, are used to generate a
  standardized \code{data.frame}, rather like a Csound
  score, that is then used by whatever method renders the
  sounds (currently only rendering in R as sine waves is
  supported).
}
\keyword{internal}

