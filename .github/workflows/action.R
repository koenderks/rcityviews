sink(file = "png/name.txt")
p <- rcityviews::cityview(name = NULL, # Take a random city
                          theme = "vintage",
                          halftone = "light",
                          filename = "png/daily.png",
                          verbose = FALSE,
                          bot = TRUE)
sink()
