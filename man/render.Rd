\name{render}
\alias{render}
\title{Render a sonification score to sound}
\usage{
  render(x, opts, file = "", audioSample = FALSE, ...)
}
\arguments{
  \item{x}{A \code{score} object created by
  \code{\link{.getScore}}}

  \item{opts}{the options of the original sonify object.
  Currently most options are ignored besides for
  \code{rendering}.}

  \item{file}{the file to save the rendering to. The
  default,\code{""}, plays the sonification in real time.}

  \item{audioSample}{Logical indicating whether to return
  an \code{audioSample} object containing the from the
  object. This is only currently available when rendering
  using \code{"audio"}.}

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

