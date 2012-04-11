x <- sonify(data.frame(rarf = 1:20, comparf = 20:1), sonaes(ref = rarf, compare = comparf)) + shape_curvepair(length = 1)

sonplay(x)


