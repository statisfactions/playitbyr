\name{getShapeDef}
\alias{getShapeDef}
\title{Get the \code{shapeDef} object for a shape}
\usage{
  getShapeDef(x)
}
\arguments{
  \item{x}{A \code{\link{sonlayer}} object or character
  string}
}
\value{
  A \code{\link{shapeDef}} object
}
\description{
  Each shape has a \code{\link{shapeDef}} object associated
  with it, which is a list specifying many aspects of the
  shape and its default; this function takes a sonlayer
  object or a character string and gets the appropriate
  shapeDef.
}

