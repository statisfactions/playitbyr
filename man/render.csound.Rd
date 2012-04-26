\name{render.csound}
\alias{csound_layer}
\alias{csound_layer.boxplot}
\alias{csound_layer.csound}
\alias{csound_layer.curvepair}
\alias{csound_layer.dotplot}
\alias{csound_layer.histogram}
\alias{csound_layer.scatter}
\alias{render.csound}
\title{Rendering sound using csound}
\usage{
  render.csound(x, opts, file = "", ...)

  csound_layer(sonlayerscore, ...)

  \method{csound_layer}{scatter} (sonlayerscore, ...)

  \method{csound_layer}{csound} (sonlayerscore, ...)

  \method{csound_layer}{dotplot} (sonlayerscore, ...)

  \method{csound_layer}{histogram} (sonlayerscore, ...)

  \method{csound_layer}{boxplot} (sonlayerscore, ...)

  \method{csound_layer}{boxplot} (sonlayerscore, ...)
}
\arguments{
  \item{\dots}{Currently ignored}

  \item{sonlayerscore}{An element of the score list--the
  score produced for a specific layer. The class of this
  determines the shape to be rendered}

  \item{x}{A \code{score} object created by
  \code{\link{.getScore}}}

  \item{opts}{the options of the original sonify object.}

  \item{file}{the file to save the rendering to. The
  default,\code{""}, plays the sonification in real time.}
}
\description{
  \code{render.csound} uses the csound package to render a
  \code{score} object.
}
\note{
  See \code{\link{createPerformance}} for possible
  \code{render_options} that can be used with this
  rendering technique; all the arguments of
  \code{createPerformance} can be passed as options.
}
\keyword{internal}

