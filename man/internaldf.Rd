\name{internaldf}
\alias{.dfNotes}
\alias{.getMappings}
\alias{.getData}
\alias{.getNotes}
\title{Internal functions to generate output \code{data.frame} from a
  \code{sonify} object.}
\usage{
.dfNotes(s)
.getMappings(x, sonlayernum)
.getData(x, sonlayernum)
.getNotes(x, sonlayernum)
}

\description{These functions, together, are used to generate a
  standardized \code{data.frame}, rather like a Csound score, that is
  then used by whatever method renders the sounds (currently only
  rendering in R as sine waves is supported).}

\arguments{
  \item{s, x}{A \code{sonify} object}
  \item{sonlayernum}{The layer number of the \code{sonify}
    object. This is a placeholder for future implementation of a
    layering functionality (modeled after \code{ggplot2} objects).}
}
\value{
  The end product of all this is a \code{data.frame} object that is used
  as an input to the rendering process}
  
\keyword{internal}
  