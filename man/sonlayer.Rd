\name{sonlayer}
\alias{shape_notes}
\alias{sonlayer}
\title{Creating individual layers for sonify objects}
\usage{
  sonlayer(shape = "notes", shape_params = NULL, stat =
  NULL, stat_params = NULL, data = NULL, mapping = NULL)

  shape_notes(...)
}
\arguments{
  \item{shape}{A character string representing the overall
  style of the audio plot (analogous to \code{geom}s from
  the \pkg{ggplot2} package).}

  \item{shape_params}{Additional parameters specific to the
  shape}

  \item{stat}{The statistic to be calculated for the layer}

  \item{stat_params}{Additional parameters specific to the
  stat}

  \item{data}{The \code{data.frame} to be sonified for this
  layer. If blank, the data from the parent \code{sonify}
  object is used.}

  \item{mapping}{A \code{\link{sonaes}} object.}

  \item{\dots}{Layer parameters to be passed to
  \code{sonlayer}}
}
\description{
  These functions are a start on the layer functionality
  for \code{sonify} objects, analogous to \pkg{ggplot2}
  layers. This creates a list object that can be included
  in a \code{sonify} object.
}
\section{Conflicting defaults on \code{sonlayer} scalings}{
  Each shape has its own default scalings (see
  \code{link{getShapeDef}} to view these defaults. It's
  quite possible that some default scalings for a parameter
  differ between scalings; if this is the case, the default
  scaling for the earlier \code{sonlayer} type, as it
  appears in the \code{$sonlayers} slot of a \code{sonify}
  object, takes precedence.

  Default scalings are, of course, ignored when they are
  explicitly declared; if you don't like the behavior of a
  default scaling you can always define your own (see
  \code{\link{sonscaling}} and
  \code{\link{scaleShortcuts}}.
}

