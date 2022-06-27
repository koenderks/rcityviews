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
    "rouge" = "#f2deb8",
    "colored" = "#eff0db",
    "neon" = "#0be8ed"
  )
  bg.col <- switch(theme,
    "original" = "#fdf9f5",
    "light" = "#fafafa",
    "dark" = "#000000",
    "rouge" = "#a25543",
    "colored" = "#eff0db",
    "neon" = "#000000"
  )
  if (theme %in% c("original", "light", "dark", "rouge")) {
    water.col <- if (theme %in% c("original", "light")) bg.col else line.col
    building.col <- bg.col
    text.col <- line.col
    rail.col <- line.col
  } else if (theme == "colored") {
    water.col <- "#b0e3cf"
    building.col <- c("#8e76a4", "#a193b1", "#db9b33", "#e8c51e", "#ed6c2e")
    text.col <- "#000000"
    rail.col <- line.col
  } else if (theme == "neon") {
    water.col <- "#ec3b8d"
    building.col <- "#000000"
    text.col <- "#e7d073"
    rail.col <- "#e7d073"
  }
  colors <- list(line.col, bg.col, water.col, building.col, text.col, rail.col)
  return(colors)
}
