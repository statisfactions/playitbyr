\name{render}
\alias{render}
\title{Render a sonification score to sound}
\usage{
  render(x, opts, file = "", ...)
}
\arguments{
  \item{x}{A \code{score} object created by
  \code{\link{.getScore}}}

  \item{opts}{the options of the original sonify object.}

  \item{file}{the file to save the rendering to. The
  default,\code{""}, plays the sonification in real time.}

  \item{\dots}{Arguments to pass to the specific render
  method.}
}
\value{
  The length of the resulting sonification
}
\description{
  \code{render} is a generic which takes a \code{score}
  object created by \code{\link{.getScore}} and renders it
  to sound.
}
\keyword{internal}

