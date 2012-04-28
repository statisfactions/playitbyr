##' Continuous note decay scales
##'
##' These shortcut functions (with the notation
##' \code{scale_}\kbd{[sound parameter]}\code{_}\kbd{[scaling
##' function]}) are intended to be added onto a \code{sonify} object
##' to specify how \kbd{[data parameter]} from the data (a column of
##' the data) is to be mapped to sonic parameters (here,
##' \code{"decayp"}, the percentage of the note devoted to the
##' decay/fade out).
##'
##' In \code{\link{shape_scatter}}, there are 3 phases of a note: the
##' attack (the initial fade-in of the note), sustain (where the note
##' is held), and the decay (the final fade-out of the
##' note. \code{decayp} is the percentage of the note devoted to the
##' decay; so a note with \code{decayp} would have a very abrupt
##' ending, whereas a note with \code{decayp = 0.5} would gradually
##' fade out.
##'
##' \code{_continuous} scales are a linear scaling, whereas
##' \code{_exp} is an exponential scale.
##'
##' @return A \code{sonscaling} object, to be used in \code{sonify} or
##' added onto a \code{sonify} object.
##' 
##' @inheritParams scale_time_continuous
##' @param \dots Other parameters (currently ignored)
##' @seealso \code{\link{shape_scatter}}, which has more information
##' about this parameter
##' @rdname scale_decayp_continuous
##' @export
scale_decayp_exp <- function(soundlimits, limits = NULL, by = NULL, ...) sonscaling(decayp = list(limits, soundlimits, function(x, limits, soundlimits) exp_scale(x, limits = limits, soundlimits, by = by)))

##' @rdname scale_decayp_continuous
##' @export
scale_decayp_continuous <- function(soundlimits, limits = NULL, by = NULL, ...) sonscaling(decayp = list(limits, soundlimits, function(x, limits, soundlimits) linear_scale(x, limits = limits, soundlimits, by = by)))


##' 
