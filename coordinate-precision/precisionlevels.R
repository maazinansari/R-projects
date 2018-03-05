coords = c(lat = 40.109201, lon = -88.226099)
# Need to adjust so right/left/top/bottom is always E/W/N/S in all hemispheres
coordinate_precision = function(coordinate) {
    coords_str = as.character(coordinate)
    coords_split = strsplit(coords_str, ".", fixed = TRUE)[[1]]
    in_decimal = coords_split[2]
    in_precision = nchar(in_decimal)
    precision = seq(0, in_precision-1)
    low_precision = sapply(precision, substr, x = in_decimal, start = 1)
    precision_range = sapply(low_precision, paste0, c("0", "9"))
    ranges = sapply(coords_split[1], paste, precision_range, sep = ".")
    return(ranges)
}

in_precision = nchar(in_decimal)
precision = seq(1, in_precision)
precisions = rep(precision, each = 2)

wrapper = lapply(coords, coordinate_precision)
bottom = seq(1, length(wrapper[["lat"]]), by = 2)
right = seq(1, length(wrapper[["lon"]]), by = 2)
top = seq(2, length(wrapper[["lat"]]), by = 2)
left = seq(2, length(wrapper[["lon"]]), by = 2)

precision_box = data.frame(precision = precision,
                           bottom = wrapper[["lat"]][bottom],
                           right = wrapper[["lon"]][right],
                           top = wrapper[["lat"]][top],
                           left = wrapper[["lon"]][left])
precision_box[,-1] = apply(precision_box[,-1], 2, as.double)