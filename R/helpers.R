.resolveIndexConflicts <- function(name, cityIndex, cities, bot) {
  if (length(cityIndex) == 0) {
    stop(paste0("There is no city called '", name, "' in the available data.\nCreate an issue including lat/long coordinates at https://github.com/koenderks/rcityviews/issues."))
  } else if (length(cityIndex) > 1) {
    if (bot) {
      index <- sample(1:length(cityIndex), size = 1)
    } else {
      index <- utils::menu(
        choices = paste0(cities[cityIndex, 1], ", ", cities[cityIndex, 2], " | Lat: ", round(cities[cityIndex, 3], 3), " | Long: ", round(cities[cityIndex, 4], 3)),
        title = "More than one city matched to this name, which one to pick?"
      )
      if (index == 0) {
        return()
      }
    }
    cityIndex <- cityIndex[index]
  }
  return(cityIndex)
}

.checkAndCrop <- function(object, crop, border) {
  if (!is.null(object)) {
    object <- sf::st_make_valid(object)
    if (border != "none") {
      sf::st_crs(object) <- sf::st_crs(crop)
      object <- object |> sf::st_intersection(crop)
    }
  }
  return(object)
}

.make_circle <- function(long, lat, rlong, rlat, npoints = 10000) {
  tt <- seq(0, 2 * pi, length.out = npoints)
  xx <- long + rlong * cos(tt)
  yy <- lat + rlat * sin(tt)
  return(data.frame(x = xx, y = yy))
}

.theme_colors <- function(theme) {
  line.col <- switch(theme,
    "original" = "#32130f",
    "light" = "#000000",
    "dark" = "#ffffff",
    "colored" = "#eff0db",
    "rouge" = "#f2deb8",
    "verde" = "#fafafa",
    "neon" = "#0be8ed"
  )
  bg.col <- switch(theme,
    "original" = "#fdf9f5",
    "light" = "#fafafa",
    "dark" = "#000000",
    "colored" = line.col,
    "rouge" = "#a25543",
    "verde" = "#6ca67a",
    "neon" = "#000000"
  )
  water.col <- switch(theme,
    "original" = bg.col,
    "light" = bg.col,
    "dark" = "#fafafa",
    "colored" = "#b0e3cf",
    "rouge" = line.col,
    "verde" = line.col,
    "neon" = "#ec3b8d"
  )
  landuse.col <- switch(theme,
    "original" = bg.col,
    "light" = bg.col,
    "dark" = bg.col,
    "colored" = c("#8e76a4", "#a193b1", "#db9b33", "#e8c51e", "#ed6c2e"),
    "rouge" = bg.col,
    "verde" = bg.col,
    "neon" = bg.col
  )
  text.col <- switch(theme,
    "original" = line.col,
    "light" = line.col,
    "dark" = line.col,
    "colored" = "#000000",
    "rouge" = line.col,
    "verde" = line.col,
    "neon" = "#e7d073"
  )
  rail.col <- switch(theme,
    "original" = line.col,
    "light" = line.col,
    "dark" = line.col,
    "colored" = line.col,
    "rouge" = line.col,
    "verde" = line.col,
    "neon" = text.col
  )
  water.line.col <- switch(theme,
    "original" = line.col,
    "light" = line.col,
    "dark" = water.col,
    "colored" = water.col,
    "rouge" = line.col,
    "verde" = line.col,
    "neon" = water.col
  )
  building.col <- switch(theme,
    "original" = bg.col,
    "light" = bg.col,
    "dark" = bg.col,
    "colored" = c("#8e76a4", "#a193b1", "#db9b33", "#e8c51e", "#ed6c2e"),
    "rouge" = bg.col,
    "verde" = line.col,
    "neon" = bg.col
  )
  colors <- list(line.col, bg.col, water.col, landuse.col, text.col, rail.col, water.line.col, building.col)
  return(colors)
}
