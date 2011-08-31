\name{render}
\alias{render}
\title{Render a sonification score to sound}
\usage{
  render(x, audioSample = FALSE, ...)
}
\arguments{
  \item{x}{A \code{score} object created by
  \code{\link{.getScore}}}

  \item{audioSample}{Logical indicating whether to return
  an \code{audioSample} object containing the from the
  object.}

  \item{\dots}{Arguments to pass to the specific render
  method.}
}
\value{
  If \code{audioSample=TRUE}, an audioSample object
  containing the sound of the rendering; otherwise NULL
}
\description{
  \code{render} is a generic which takes a \code{score}
  object created by \code{\link{.getScore}} and renders it
  to sound.
}
\details{
  The class of the \code{score} object \code{x}
  determinines object, which determines the render method
  called. Currently only \code{"audio"} is available for a
  rendering method, but plans for csound are in the works.
}
\keyword{internal}

