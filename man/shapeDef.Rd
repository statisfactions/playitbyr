\name{shapeDef}
\alias{shapeDef}
\alias{shapeParam}
\title{Construction functions for a new \code{shapedef}}
\usage{
  shapeDef(shapename, renderings, params)

  shapeParam(param, min, max, defaultSetting,
  defaultScaling, description)
}
\arguments{
  \item{shape}{A string containing name of the shape (e.g.
  \sQuote{\code{notes}})}

  \item{renderings}{A character vector containing the
  rendering methods (see \code{link{rendering}}) that
  support this shape.}

  \item{params}{A named list of the sound parameters
  created by newParam}

  \item{param}{the name of the individual parameter}

  \item{min}{The minimum value that this parameter can
  take}

  \item{max}{The maximum value that this parameter can
  take}

  \item{defaultSetting}{The default numeric setting of this
  variable, used if no setting or mapping is specified.}

  \item{defaultScaling}{The default scaling (as created by
  \code{\link{sonscaling}} for this parameter}

  \item{description}{A string containing the description of
  the parameter}

  \item{errors}{A list of expression objects containing an
  \code{if} statement, a condition for the error, and a
  string with the error message. This is used by
  \code{\link{checkSonify}} to check any special conditions
  for this parameter.}
}
\value{
  \code{shapeDef} returns a new shapeDef object that for
  use by \code{\link{checkSonify}} and
  \code{\link{getShapeDef}}; \code{shapeParam} returns a
  named list for use in the \code{params} argument of
  \code{shapeDef}
}
\description{
  These functions facilitate the creation of a new kind of
  \code{shape} by providing functions to construct a new
  \code{shapeDef} object which contains everything
  \pkg{playitbyr} needs to keep track of mappings,
  scalings, and error-checking for the shape.
}

