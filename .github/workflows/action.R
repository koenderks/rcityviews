cities <- subset(rcityviews::cities, rcityviews::cities$population > 100000)

city <- cities[sample(1:nrow(cities), size = 1), ]
border <- sample(x = c("circle", "rhombus", "hexagon", "octagon", "decagon"), size = 1)
theme <- sample(x = c("original", "light", "dark", "destination", "rouge", "colored", "neon"), size = 1)

sink(file = "png/name.txt")
p <- rcityviews::cityview(name = "Amsterdam", theme = "colored", border = "circle",
                          filename = "png/daily.png", verbose = FALSE, bot = TRUE)
# p <- rcityviews::cityview(name = city[["name"]], theme = theme, border = border,
#                           filename = "png/daily.png", verbose = FALSE, bot = TRUE)
sink()
