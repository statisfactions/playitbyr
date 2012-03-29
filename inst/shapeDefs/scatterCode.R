time <- shapeParam("time",
                   0,
                   Inf,
                   0,
                   list(limits = NULL, soundlimits = c(0, 5), linear_scale),
                   NULL,
                   "The starting time. Either time or tempo, but not both, must be specified for an object to be rendered.")
pitch <- shapeParam("pitch",
                    4,
                    15,
                    8,
                    list(limits = NULL, soundlimits = c(8, 9), function(x, limits, soundlimits) linear_scale(x, limits = limits, soundlimits, by = 1/12)),
                    NULL,
                    "Pitch represented such that 8 represents middle C, 9 represents the octave above, etc. (This is Csound's 'oct' notation.) By default this is scaled to the nearest musical pitch.")
dur <- shapeParam("dur",
                  0,
                  Inf,
                  2,
                  list(limits = NULL, soundlimits = c(0.25, 4), linear_scale),
                  NULL,
                  "The relative desired duration of all events in beats, where 1 beat equals the length of time for one event if all events were equal length")
vol <- shapeParam("vol",
                  0,
                  1,
                  0.5,
                  list(limits = NULL, soundlimits = c(0.2, 1), linear_scale),
                  NULL,
                  "The desired volume of all events as a number between 0, silence, and 1, the maximum possible amplitude.")
pan <- shapeParam("pan",
                  0,
                  1,
                  0.5,
                  list(limits = NULL, soundlimits = c(0, 1), linear_scale),
                  NULL,
                  "The desired balance between left and right stereo channels, where 0 is all the left channel, 0.5 balanced, and 1 all the right.")
tempo <- shapeParam("tempo",
                    0,
                    Inf,
                    NULL,
                    list(limits = NULL, soundlimits = c(120, 240), linear_scale),
                    NULL,
                    "The desired tempo of events in beats per minute. Either time or tempo, but not both, must be specified for an object to be rendered.")

attkp <- shapeParam("attkp",
                    0,
                    1,
                    0.01,
                    list(limits = NULL, soundlimits = c(0.01, 0.3), linear_scale),
                    NULL,
                    "The desired proportion of the note\'s length devoted to the initial (linear) attack.")

decayp <- shapeParam("decayp",
                    0,
                    1,
                    0.01,
                    list(limits = NULL, soundlimits = c(0.01, 0.3), linear_scale),
                    NULL,
                    "The proportion of the note\'s length devoted to the (linear) decay.")
mod <- shapeParam("mod",
                    0,
                    Inf,
                    1,
                    list(limits = NULL, soundlimits = c(0.8, 5), linear_scale),
                    NULL,
                    "The modulating frequency, given as a multiple of the carrier tone.")
indx <- shapeParam("indx",
                    0,
                    1,
                    0.01,
                    list(limits = NULL, soundlimits = c(0.01, 0.3), linear_scale),
                    NULL,
                    "The index of modulation.")

scatter <- shapeDef("A scatterplot shape; sine tones in rendering 'audio', FM synthesis in rendering 'csound'", "audio", list(), c(time, pitch, dur, vol, pan, tempo, attkp, decayp, mod, indx))

                                
                                
