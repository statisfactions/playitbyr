##' Continuous scales for note attacks
##'
##' These shortcut functions (with the notation
##' \code{scale_}\kbd{[sound parameter]}\code{_}\kbd{[scaling
##' function]}) are intended to be added onto a \code{sonify} object
##' to specify how \kbd{[data parameter]} from the data (a column of
##' the data) is to be mapped to sonic parameters (here,
##' \code{"attkp"}, the sharpness of a note's initial attack).
##'
##' In \code{\link{shape_scatter}}, there are 3 phases of a note: the
##' attack (the initial fade-in of the note), sustain (where the note
##' is held), and the decay (the final fade-out of the
##' note. \code{attkp} is the percentage of the note devoted to the
##' attack; so a note with \code{attkp = 0.01} would have a very quick
##' start of the note, whereas a note with \code{attkp = 0.5} would
##' gradually fade in.
##'
##' \code{_continuous} scales are a linear scaling, whereas
##' \code{_exp} is an exponential scale.
##'
##' @return A \code{sonscaling} object, to be used in \code{sonify} or
##' added onto a \code{sonify} object.
##'
##' @inheritParams scale_time_continuous
##' @seealso \code{\link{shape_scatter}}, which has more information
##' about this parameter
##' @param \dots Other parameters (currently ignored)
##' @rdname scale_attkp_continuous
##' @export
scale_attkp_continuous <- function(soundlimits, limits = NULL, by = NULL, ...) sonscaling(attkp = list(limits, soundlimits, function(x, limits, soundlimits) linear_scale(x, limits = limits, soundlimits, by = by)))

##' @rdname scale_attkp_continuous
##' @export
scale_attkp_exp <- function(soundlimits, limits = NULL, by = NULL, ...) sonscaling(attkp = list(limits, soundlimits, function(x, limits, soundlimits) exp_scale(x, limits = limits, soundlimits, by = by)))


##' 
