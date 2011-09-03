\name{getShapeDef}
\alias{getShapeDef}
\alias{getShapeNames}
\title{Get the \code{shapeDef} object for a shape}
\usage{
  getShapeDef(x)

  getShapeNames()
}
\arguments{
  \item{x}{A \code{\link{sonlayer}} object or character
  string}
}
\value{
  \code{getShapeDef} returns a \code{shapeDef} object;
  \code{getShapeNames} returns a character vector
  containing the names of all currently available shapes
}
\description{
  Each shape has a \code{shapeDef} object associated with
  it, which is a list specifying many aspects of the shape
  and its default; this function takes a sonlayer object or
  a character string and gets the appropriate shapeDef.
}
\keyword{internal}

