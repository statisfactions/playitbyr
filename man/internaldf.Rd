\name{internaldf}
\alias{.getScore}
\alias{.getSonlayerData}
\alias{.getSonlayerMappings}
\alias{.getSonlayerScore}
\alias{.getSonlayerShape}
\alias{.getSonlayerStat}
\alias{internaldf}
\alias{.rescaleDataByParam}
\title{Internal functions to generate output \code{data.frame} from a
\code{sonify} object.}
\usage{
  .getScore(x)

  .getSonlayerScore(x, sonlayernum)

  .getSonlayerMappings(x, sonlayernum, remove.null = TRUE)

  .getSonlayerData(x, sonlayernum, transform = TRUE)

  .getSonlayerStat(x, sonlayernum)

  .getSonlayerShape(x, sonlayernum)

  .rescaleDataByParam(x, param, column)
}
\arguments{
  \item{x}{A \code{sonify} object}

  \item{sonlayernum}{The layer number of the \code{sonify}
  object. This is a placeholder for future implementation
  of a layering functionality (modeled after \code{ggplot2}
  objects).}

  \item{remove.null}{Logical indicating whether to remove
  missing mappings from the returned list of mappings.
  .getSonlayerScore calls this with TRUE to avoid
  cluttering calculations; but checkSonify calls this with
  FALSE since it bases its approach on having the null
  slots in.}

  \item{transform}{A logical indicating whether to perform
  the given statistical transformation (stat) to the
  layer.}

  \item{param}{The sound parameter}

  \item{column}{The data.frame column (vector) to be
  rescaled}
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
\note{
  These functions (particularly .getSonlayerScore) assume
  that every shape has a single output row for each input
  data row after applying statistical tranformations
}
\section{Not yet implemented}{
  .getStat currently just returns NULL. It is intended that
  it will eventually return a function that
  .getSonlayerData can use to transform a data.frame, or
  NULL if no tranformation is requested. This function (and
  its only call in .getSonlayerData) will likely change
  soon.
}
\keyword{internal}

