# Copyright (C) 2022-2022 Koen Derks

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

.getCity <- function(name) {
  if (is.null(name)) {
    city <- .randomCity(NULL)
  } else {
    dataset <- rcityviews::cities
    indexes <- which(dataset[["name"]] == name)
    index <- .resolveConflicts(name, indexes, dataset)
    if (is.null(index)) {
      return(NULL)
    }
    city <- dataset[index, ]
  }
}

.randomCity <- function(seed) {
  set.seed(seed)
  dataset <- rcityviews::cities
  dataset <- subset(dataset, dataset[["population"]] > 250000)
  index <- sample(1:nrow(dataset), size = 1)
  selected <- dataset[index, ]
  return(selected)
}

.resolveConflicts <- function(name, indexes, dataset) {
  index <- indexes
  if (length(indexes) == 0) {
    stop(paste0("There is no city called '", name, "' in the available data.\nCreate an issue including lat/long coordinates at https://github.com/koenderks/rcityviews/issues."))
  } else if (length(indexes) > 1) {
    selection <- utils::menu(
      choices = paste0(dataset[indexes, 1], ", ", dataset[indexes, 2], " | Lat: ", round(dataset[indexes, 3], 3), " | Long: ", round(dataset[indexes, 4], 3)),
      title = "More than one city matched to this name, which one to pick?"
    )
    if (selection == 0) {
      return(NULL)
    }
    index <- indexes[selection]
  }
  return(index)
}

.tick <- function(verbose, progBar, ticks, shiny) {
  if (shiny) {
    shiny::incProgress(amount = 1 / ticks)
  } else {
    if (verbose) {
      progBar$tick()
    }
  }
}

.themeOptions <- function(theme) {
  colors <- switch(theme,
    "vintage" = list(
      "background" = "#fff7d8",
      "water" = "#9ebfaa",
      "landuse" = "#fff7d8",
      "contours" = "#32130f",
      "streets" = "#32130f",
      "rails" = c("#32130f", "#fff7d8"),
      "buildings" = c("#facc87", "#f39848", "#f8c98c", "#f58762"),
      "text" = "#32130f",
      "waterlines" = "#9ebfaa"
    ),
    "modern" = list(
      "background" = "#e6ddd6",
      "water" = "#656c7c",
      "landuse" = "#7c9c6b",
      "contours" = "#e6ddd6",
      "streets" = "#fafafa",
      "rails" = c("#fafafa", "#e6ddd6"),
      "buildings" = "#eb3e20",
      "text" = "#000000",
      "waterlines" = "#656c7c"
    ),
    "bright" = list(
      "background" = "#eeefc9",
      "water" = "#9ddffb",
      "landuse" = c("#f2f4cb", "#d0f1bf", "#64b96a"),
      "contours" = "#eeefc9",
      "streets" = "#2f3737",
      "rails" = c("#2f3737", "#eeefc9"),
      "buildings" = c("#8e76a4", "#a193b1", "#db9b33", "#e8c51e", "#ed6c2e"),
      "text" = "#2f3737",
      "waterlines" = "#9ddffb"
    ),
    "delftware" = list(
      "background" = "#fafafa",
      "water" = "#fafafa",
      "landuse" = c("#7ebaee", "#8da8d7", "#3259a6", "#0c133f", "#080e1c"),
      "contours" = "#fafafa",
      "streets" = "#1F305E",
      "rails" = c("#1F305E", "#fafafa"),
      "buildings" = c("#7ebaee", "#8da8d7", "#3259a6", "#0c133f", "#080e1c"),
      "text" = "#000000",
      "waterlines" = "#fafafa"
    ),
    "comic" = list(
      "background" = "#ffffff",
      "water" = "#607ba4",
      "landuse" = "#4b9475",
      "contours" = "#222222",
      "streets" = "#222222",
      "rails" = c("#222222", "#ffffff"),
      "buildings" = c("#f4d749", "#daa520", "#a63c44"),
      "text" = "#222222",
      "waterlines" = "#607ba4"
    ),
    "rouge" = list(
      "background" = "#a25543",
      "water" = "#f2deb8",
      "landuse" = "#a25543",
      "contours" = "#f2deb8",
      "streets" = "#f2deb8",
      "rails" = c("#f2deb8", "#a25543"),
      "buildings" = "#f2deb8",
      "text" = "#f2deb8",
      "waterlines" = "#f2deb8"
    ),
    "original" = list(
      "background" = "#fdf9f5",
      "water" = "#fdf9f5",
      "landuse" = "#fdf9f5",
      "contours" = "#32130f",
      "streets" = "#32130f",
      "rails" = "#32130f",
      "buildings" = "#fdf9f5",
      "text" = "#32130f",
      "waterlines" = "#32130f"
    )
  )
  font <- switch(theme,
    "vintage" = list(
      "family" = "Fredericka the Great",
      "face" = "plain"
    ),
    "modern" = list(
      "family" = "Imbue",
      "face" = "plain"
    ),
    "bright" = list(
      "family" = "Damion",
      "face" = "plain"
    ),
    "delftware" = list(
      "family" = "Dancing Script",
      "face" = "bold"
    ),
    "comic" = list(
      "family" = "Rampart One",
      "face" = "plain"
    ),
    "rouge" = list(
      "family" = "Oswald",
      "face" = "bold"
    ),
    "original" = list(
      "family" = "Caveat",
      "face" = "bold"
    )
  )
  size <- list()
  size[["borders"]] <- list(
    "contours" = 0.3,
    "water" = 0.4,
    "canal" = 0.5,
    "river" = 0.6
  )
  size[["streets"]] <- list(
    "path" = 0.2,
    "residential" = 0.4,
    "structure" = 0.5,
    "tertiary" = 0.7,
    "secondary" = 0.8,
    "primary" = 0.9,
    "motorway" = 1,
    "rails" = 0.75,
    "runway" = 3
  )
  themeOptions <- list(
    "colors" = colors,
    "font" = font,
    "size" = size
  )
  return(themeOptions)
}
