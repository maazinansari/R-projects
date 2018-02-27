par(mfrow = c(3,3), mar = c(1,1,2,1), bg = "gray15")
x = y = 0:10
alignments = expand.grid(c("Lawful", "Neutral", "Chaotic"),
                         c("Good", "Neutral", "Evil"))

alignments$type = paste(alignments[,1], alignments[,2])
alignments$type[5] = "True Neutral"

# Routes ----
routes = list(yellow = list(name = "1/100-Yellow-Yellowhopper",
                            bg = "#fff43a",
                            tc = "black",
                            desc = "\"savoy walmart here we come\""),
              red = list(name = "2/20-Red",
                         bg = "#e22922",
                         tc = "white",
                         desc = "\"where tf are we going\""),
              green = list(name = "5/50-Green-Greenhopper-Green Express",
                           bg = "#2e6f50",
                           tc = "white",
                           desc = "\"icard please\""),
              orange = list(name = "6-Orange-Orangehopper",
                            bg = "#ec8d2b",
                            tc = "black",
                            desc = "lincoln square to illinois terminal. simple."),
              brown = list(name = "9A/9B-Brown",
                           bg = "#6e441a",
                           tc = "white",
                           desc = "\"where tf are we going\""),
              gold = list(name = "10-Gold-Goldhopper",
                          bg = "#bc893b",
                          tc = "black",
                          desc = "the unsung hero of the ike"),
              teal = list(name = "12/120-Teal",
                          bg = "#21557e",
                          tc = "white",
                          desc = "eceb to par late night"),
              silver = list(name = "13/130-Silver",
                            bg = "#c7c8ca",
                            tc = "black",
                            desc = "evenings and weekends, rain or shine"),
              raven = list(name = "21-Raven",
                           bg = "black",
                           tc = "white",
                           desc = "literally just goes in circles"),
              illini = list(name = "22/220-Illini-Illini Limited",
                            bg = "#461147",
                            tc = "white",
                            desc = "no icard needed"),
              safe = list(name = "335-Safe Rides",
                          bg = "#ecf2d3",
                          tc = "black",
                          desc = "\"please wait 45 min. alone. in the dark.\""))
# Sorting ----
# Sort 9 routes
# Row first, then column (lawful-chaotic, good-evil)
routes_9 = list(routes$silver, routes$teal,  routes$illini,
                routes$gold,   routes$raven, routes$safe,
                routes$yellow, routes$green, routes$brown)

for (i in 1:9) {
    plot(x, y,
         main = alignments$type[i],
         cex.main = 1.5,
         col.main = "white",
         type = "n",
         xlab = "",
         ylab = "",
         axes = FALSE)
    rect(0, 0, 10, 10, col = routes_9[[i]][["bg"]], border = NA)
    text(x = 5, y = 6,
         labels = gsub("-", "\n", routes_9[[i]][["name"]]),
         col = routes_9[[i]][["tc"]],
         font = 2,
         cex = 1.9)
    # text(x = 5, y = 4,
    #      labels = routes_9[[i]][["desc"]],
    #      col = routes_9[[i]][["tc"]])
}
