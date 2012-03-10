inst <- shapeParam("inst", 1, Inf, 1, NULL, NULL, "Instrument number in csound score")
start <- shapeParam("start", 0, Inf, 0, list(limits = NULL, soundlimits = c(0, 5), linear_scale), NULL, "Starting time")
dur <- shapeParam("dur", 0, Inf, 2, NULL, list(limits = NULL, soundlimits = c(0.25, 4), linear_scale), "Duration in seconds")
pfield <- shapeParam("p", NULL, NULL, NULL, NULL, NULL, "Arbitrary p field")

pto20 <- rep(pfield, 17)
names(pto20) <-paste("p", 4:20, sep = "")
params <- c(inst, start, dur, pto20)

csound <- shapeDef("Arbitrary csound instrument", "csound", list(f = NULL), params)
