library(dplyr)
library(TSP)

pts = read.csv("image-tsp/rand-pts-robinson-2500.csv")[,-1]

# K-means
kcont = kmeans(pts, centers = 7)
cont_color = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", 
               "#fb9a99", "#e31a1c", "#fdbf6f",
               "#ff0000", "#00ff00", "#0000ff",
               "#00ffff", "#ff00ff", "#ffff00")
plot(pts$xcoord, pts$ycoord, pch = 20, col = cont_color[kcont$cluster])

# Hierarchical
hcluster = hclust(dist(pts), method = "average")
hcont = cutree(hcluster, k = 7)
xo = pts$xcoord[order(hcont)]
yo = pts$ycoord[order(hcont)]
plot(pts$xcoord, pts$ycoord, pch = 20, col = cont_color[hcont])
plot(xo, yo, col = "gray", type = "l")

# Density-based
library(dbscan)
dbcluster = dbscan(pts, eps = 0.07, minPts = 5)
plot(pts$xcoord, pts$ycoord, pch = 20, col = cont_color[dbcluster$cluster])

# Compute distances and solve TSP (it may take a minute)
generate_tsp = function(pts) {
    as.TSP(dist(pts)) %>% 
        solve_TSP(method = "arbitrary_insertion") %>% 
        as.integer() -> solution
    
    # Create a dataframe with the output of TSP
    data.frame(id=solution) %>% 
        mutate(order=row_number()) -> order
    
    # Rearrange the original points according the TSP output
    # pts %>% 
    #   mutate(id=row_number()) %>% 
    #   inner_join(order, by="id") %>% arrange(order) %>% 
    #   select(xcoord,ycoord) -> data_to_plot
    
    pts_ordered = pts[order$id,]
    
    plot(x = pts_ordered$xcoord,
         y = pts_ordered$ycoord,
         type = "l",
         lwd = 1,
         lty = 1,
         pch = 4,
         #col = "gray",
         axes = FALSE,
         ylab = "",
         xlab = "")
}

generate_tsp(pts)

