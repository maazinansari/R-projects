# map x in the range [a,b] to y in [c,d]
# y=(x-a)\frac{d-c}{b-a}+c
maprange = function(x, a, b, c = 0, d = 1) {
    y = (x - a) * (d - c) / (b - a) + c
    return(y)
}

# maprange(12, 11, 13)
# maprange(-1.5, -1, -2, 1, 2)

plotmap1 = function(x, a, b, c, d) {
    
    y = maprange(x, a, b, c, d)
    
    largerange = max(b - a, d - c)
    min_start = min(a, c) - 0.1 * largerange
    max_stop = max(b, d) + 0.1 * largerange
    
    plot(x = c(min_start, max_stop),
         y = c(0, 2),
         type = "n",
         axes = FALSE, 
         xlab = "",
         ylab = "")
    
    # input
    lines(x = c(a, b), y = rep(1.5, 2))
    text(x = c(a, b, x), y = rep(1.5, 3),
         pos = c(1, 1, 3),
         labels = round(c(a, b, x), 2))
    
    # transformation
    lines(x = c(c, d), y = rep(0.5, 2))
        text(x = c(c, d, y), y = rep(0.5, 3),
         pos = c(3, 3, 1),
         labels = round(c(c, d, y), 2))
    
    # connect
    segments(x0 = x, y0 = 1.5, x1 = y, y1 = 0.5, lty = 3)
}

plotmap2 = function(x, a, b, c, d) {
    y = maprange(x, a, b, c, d)
    
    par(mfrow = c(2, 1))
    plot(x = c(a, b),
         y = rep(0, 2),
         ylim = c(-1, 1),
         ylab = "",
         xlab = "",
         axes = FALSE,
         type = "b",
         pch = 19,
         col = "red")
    axis(1, seq(a, b))
    points(x = x,
           y = rep(0, length(x)),
           pch = 18)
    
    plot(x = c(c, d),
         y = rep(0, 2),
         ylim = c(-1, 1),
         ylab = "",
         xlab = "",
         axes = FALSE,
         type = "b",
         pch = 19,
         col = "red")
    axis(1, seq(c, d))
    points(x = y,
           y = rep(0, length(y)),
           pch = 18)
}

# Days of the year
plotmap1(127, 1, 365, 0, 100)
