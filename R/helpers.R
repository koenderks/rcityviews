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
    "destination" = "#365d65",
    "rouge" = "#f2deb8",
    "colored" = "#eff0db",
    "neon" = "#0be8ed"
  )
  bg.col <- switch(theme,
    "original" = "#fdf9f5",
    "light" = "#fafafa",
    "dark" = "#000000",
    "destination" = "#dec84e",
    "rouge" = "#a25543",
    "colored" = "#eff0db",
    "neon" = "#000000"
  )
  water.col <- switch(theme,
    "original" = bg.col,
    "light" = bg.col,
    "dark" = "#fafafa",
    "destination" = line.col,
    "rouge" = line.col,
    "colored" = "#b0e3cf",
    "neon" = "#ec3b8d"
  )
  landuse.col <- switch(theme,
    "original" = bg.col,
    "light" = bg.col,
    "dark" = bg.col,
    "destination" = bg.col,
    "rouge" = bg.col,
    "colored" = c("#8e76a4", "#a193b1", "#db9b33", "#e8c51e", "#ed6c2e"),
    "neon" = bg.col
  )
  text.col <- switch(theme,
    "original" = line.col,
    "light" = line.col,
    "dark" = line.col,
    "destination" = line.col,
    "rouge" = line.col,
    "colored" = "#000000",
    "neon" = "#e7d073"
  )
  rail.col <- switch(theme,
    "original" = line.col,
    "light" = line.col,
    "dark" = line.col,
    "destination" = line.col,
    "rouge" = line.col,
    "colored" = line.col,
    "neon" = text.col
  )
  water.line.col <- switch(theme,
    "original" = line.col,
    "light" = line.col,
    "dark" = water.col,
    "destination" = water.col,
    "rouge" = water.col,
    "colored" = water.col,
    "neon" = water.col
  )
  colors <- list(line.col, bg.col, water.col, landuse.col, text.col, rail.col, water.line.col)
  return(colors)
}
