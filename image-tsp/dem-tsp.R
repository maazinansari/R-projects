# ASTER GDEM data from: https://asterweb.jpl.nasa.gov/gdem.asp


library(imager)
library(dplyr)
library(scales)
library(TSP)
library(raster)
library(rgdal)
library(sp)

DEM = raster("image-tsp/GDEM-10km-BW.tif")

# Reduce spatial resolution
DEM_agg = aggregate(x = DEM, fact = 5)

# Save as png
DEM_agg_png = "image-tsp/DEM_agg.png"
png(DEM_agg_png)
image(DEM_agg,
      col = gray.colors(2, start = 1, end = 0),
      xlab = "", ylab = "",
      bty = "n",
      axes = FALSE)
dev.off()

# Use png as image
load.image(DEM_agg_png) %>% 
    as.cimg() %>%
    as.data.frame() %>%
    sample_n(8000, weight=(1-value)) %>%
    select(x,y) -> data

pts = data
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
         lwd = 1,
         lty = 1,
         pch = 4,
         #col = "gray",
         axes = FALSE,
         ylab = "",
         xlab = "")
}

generate_tsp(data)