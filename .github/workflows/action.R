cities <- subset(rcityviews::cities, rcityviews::cities$population > 50000)

city <- cities[sample(1:nrow(cities), size = 1), ]

sink(file = "png/name.txt")
p <- rcityviews::cityview(name = city[["name"]], 
                          theme = sample(x = c("original", "light", "dark", "rouge", "verde", "colored", "neon", "atlantis", "vintage", "lichtenstein"), size = 1),
                          border = sample(x = c("circle", "rhombus", "hexagon", "octagon", "decagon"), size = 1),
                          halftone = sample(x = c("none", "light", "dark"), size = 1, prob = c(0.5, 0.25, 0.25)),
                          places = 10,
                          filename = "png/daily.png",
                          verbose = FALSE,
                          bot = TRUE)
sink()
