sink(file = "png/name.txt")
p <- rcityviews::cityview(name = NULL, # Take a random city
                          theme = sample(x = c("vintage", "bright", "delftware", "original", "modern", "rouge"), size = 1),
                          filename = "png/daily.png",
                          verbose = FALSE,
                          bot = TRUE)
sink()
