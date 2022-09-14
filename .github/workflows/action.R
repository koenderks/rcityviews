cities <- subset(rcityviews::cities, rcityviews::cities$population > 50000)

city <- cities[sample(1:nrow(cities), size = 1), ]
border <- sample(x = c("circle", "rhombus", "hexagon", "octagon", "decagon"), size = 1)
theme <- sample(x = c("original", "light", "dark", "rouge", "verde", "colored", "neon", "atlantis", "vintage", "lichtenstein"), size = 1)
halftone <- sample(x = c("none", "auto"), size = 1)
places <- sample(x = c(0, 25), size = 1, prob = c(0.75, 0.25))

sink(file = "png/name.txt")
p <- rcityviews::cityview(name = city[["name"]], 
                          theme = theme,
                          border = border,
                          halftone = halftone,
                          places = places,
                          filename = "png/daily.png",
                          verbose = FALSE,
                          bot = TRUE)
sink()
