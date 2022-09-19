sink(file = "png/name.txt")
p <- rcityviews::cityview(name = NULL, # Take a random city
                          theme = sample(x = c("rouge", "colored", "delftware", "vintage"), size = 1),
                          halftone = "light",
                          filename = "png/daily.png",
                          verbose = FALSE,
                          bot = TRUE)
sink()
