\name{sonlayer}
\alias{sonlayer}
\title{Creating individual layers for sonify objects}
\usage{
  sonlayer(shape = "scatter", stat = NULL,
    stat_params = NULL, data = NULL, mapping = NULL, ...)
}
\arguments{
  \item{shape}{A character string representing the overall
  style of the audio plot (analogous to \code{geom}s from
  the \pkg{ggplot2} package).}

  \item{stat}{The statistic to be calculated for the layer
  (currently ignored)}

  \item{stat_params}{Additional parameters specific to the
  stat (currently ignored)}

  \item{data}{The \code{data.frame} to be sonified for this
  layer. If blank, the data from the parent \code{sonify}
  object is used.}

  \item{mapping}{A \code{\link{sonaes}} object.}

  \item{\dots}{Additional options and settings specific to
  the shape}
}
\description{
  This function creates layers to be added onto a
  \code{sonify} object.
}
\section{Conflicting defaults on \code{sonlayer} scalings}{
  Each shape has its own default scalings (see
  \code{link{.getShapeDef}} to view these defaults. It's
  quite possible that some default scalings for a parameter
  differ between scalings; if this is the case, the default
  scaling for the earlier \code{sonlayer} type, as it
  appears in the \code{$sonlayers} slot of a \code{sonify}
  object, takes precedence.

  Default scalings are, of course, ignored when they are
  explicitly declared; if you don't like the behavior of a
  default scaling you can always define your own (see
  \code{\link{sonscaling}}.
}
\seealso{
  This functionality is most easily accessed through its
  shortcut functions, the \code{shape_}\kbd{shapename}
  functions, currently: \code{\link{shape_scatter}}
}

