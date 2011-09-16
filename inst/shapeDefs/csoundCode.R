inst <- shapeParam("inst", 1, Inf, 1, NULL, NULL, "Instrument number in csound score")
start <- shapeParam("start", 0, Inf, 0, list(0, 5, linear.scale), NULL, "Starting time")
dur <- shapeParam("dur", 0, Inf, 2, NULL, list(0.25, 4, linear.scale), "Duration in seconds")
pfield <- shapeParam("p", NULL, NULL, NULL, NULL, NULL, "Arbitrary p field")

pto20 <- rep(pfield, 17)
names(pto20) <-paste("p", 4:20, sep = "")
params <- c(inst, start, dur, pto20)

csound <- shapeDef("Arbitrary csound instrument", "csound", list(orcpath=NULL, orctext=NULL, flags=NULL), params)
