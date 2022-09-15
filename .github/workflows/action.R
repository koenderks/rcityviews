sink(file = "png/name.txt")
p <- rcityviews::cityview(name = NULL, # Take a random city
                          zoom = sample(x = c(1, 0.75, 0.5, 0.25), size = 1),
                          theme = sample(x = c("original", "light", "dark", "rouge", "verde", "colored", "neon", "atlantis", "vintage", "lichtenstein"), size = 1),
                          border = sample(x = c("circle", "rhombus", "hexagon", "octagon", "decagon"), size = 1),
                          halftone = sample(x = c("none", "light", "dark"), size = 1, prob = c(0.5, 0.25, 0.25)),
                          places = sample(x = c(0, 10), size = 1, prob = c(0.75, 0.25)),
                          filename = "png/daily.png",
                          verbose = FALSE,
                          bot = TRUE)
sink()
