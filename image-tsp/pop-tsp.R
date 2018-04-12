# Gridded Population of the World data from http://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-count-rev10

library(imager)
library(dplyr)
library(scales)
library(TSP)
library(raster)
library(rgdal)
library(sp)

pop = raster("image-tsp/gpw-v4-population-count-rev10_2015_30_min_tif/gpw_v4_population_count_rev10_2015_30_min.tif")



# Reduce spatial resolution, log-transform
pop_agg = aggregate(x = pop + 0.01, fact = 1.5) %>% 
    log()

# Save as png
pop_agg_png = "image-tsp/pop_agg_png.png"
png(pop_agg_png)
image(pop_agg,
      col = gray.colors(32, start = 1, end = 0),
      xlab = "", ylab = "",
      bty = "n",
      axes = FALSE)
dev.off()

# Use png as image
load.image(pop_agg_png) %>% 
    as.cimg() %>%
    as.data.frame() %>%
    sample_n(8000, weight=(1-value)) %>%
    select(x,y) -> data

# Compute distances and solve TSP (it may take a minute)
generate_tsp = function(pts) {
    as.TSP(dist(pts)) %>% 
        solve_TSP(method = "arbitrary_insertion") %>% 
        as.integer() -> solution
    
    # Create a dataframe with the output of TSP
    data.frame(id=solution) %>% 
        mutate(order=seq(1,nrow(.))) -> order
    
    # Rearrange the original points according the TSP output
    # pts %>%
    #   mutate(id=row_number()) %>%
    #   inner_join(order, by="id") %>% arrange(order) %>%
    #   select(x,y) -> pts_ordered
    
    pts_ordered = pts[order$id,]
    maxy = max(pts_ordered$y)
    plot(x = pts_ordered$x,
         y = maxy - pts_ordered$y,
         type = "l",
         lwd = 0.5,
         lty = 1,
         pch = 4,
         #col = "gray",
         axes = FALSE,
         ylab = "",
         xlab = "")
}

generate_tsp(data)
