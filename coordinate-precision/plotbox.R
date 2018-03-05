source("precisionlevels.R")

plot(coords[2], coords[1], xlim = c(-89, -88), ylim = c(40, 41))

bounding_box = function(precision) {
    # Bottom
    lines(x = unlist(precision_box[precision, c(3,5)]),
          y = rep(unlist(precision_box[precision, "bottom"]), 2))
    # Left
    lines(y = unlist(precision_box[precision, c(2,4)]),
          x = rep(unlist(precision_box[precision, "left"]), 2))
    # Top
    lines(x = unlist(precision_box[precision, c(3,5)]),
          y = rep(unlist(precision_box[precision, "top"]), 2))
    # Right
    lines(y = unlist(precision_box[precision, c(2,4)]),
          x = rep(unlist(precision_box[precision, "right"]), 2))
}

bounding_box(2)
