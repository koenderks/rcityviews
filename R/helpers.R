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
  lines <- switch(theme,
    "original" = "#32130f",
    "light" = "#000000",
    "dark" = "#ffffff",
    "colored" = "#eff0db",
    "rouge" = "#f2deb8",
    "verde" = "#284566",
    "neon" = "#0be8ed",
    "vintage" = "#32130f",
    "delftware" = "#ffffff",
    "lichtenstein" = "#2f3737"
  )
  background <- switch(theme,
    "original" = "#fdf9f5",
    "light" = "#fafafa",
    "dark" = "#000000",
    "colored" = lines,
    "rouge" = "#a25543",
    "verde" = "#6ca67a",
    "neon" = "#000000",
    "vintage" = "#fff7d8",
    "delftware" = lines,
    "lichtenstein" = "#ffffff"
  )
  water <- switch(theme,
    "original" = background,
    "light" = background,
    "dark" = "#fafafa",
    "colored" = "#b0e3cf",
    "rouge" = lines,
    "verde" = lines,
    "neon" = "#ec3b8d",
    "vintage" = "#9ebfaa",
    "delftware" = lines,
    "lichtenstein" = "#607ba4"
  )
  waterlines <- switch(theme,
    "original" = lines,
    "light" = lines,
    "dark" = water,
    "colored" = water,
    "rouge" = lines,
    "verde" = lines,
    "neon" = water,
    "vintage" = water,
    "delftware" = lines,
    "lichtenstein" = water
  )
  landuse <- switch(theme,
    "original" = background,
    "light" = background,
    "dark" = background,
    "colored" = c("#8e76a4", "#a193b1", "#db9b33", "#e8c51e", "#ed6c2e"),
    "rouge" = background,
    "verde" = lines,
    "neon" = background,
    "vintage" = background,
    "delftware" = c("#7ebaee", "#8da8d7", "#3259a6", "#0c133f", "#080e1c"),
    "lichtenstein" = "#478f70"
  )
  text <- switch(theme,
    "original" = lines,
    "light" = lines,
    "dark" = lines,
    "colored" = "#000000",
    "rouge" = lines,
    "verde" = lines,
    "neon" = "#e7d073",
    "vintage" = lines,
    "delftware" = "#000000",
    "lichtenstein" = lines
  )
  rails <- switch(theme,
    "original" = lines,
    "light" = lines,
    "dark" = lines,
    "colored" = lines,
    "rouge" = lines,
    "verde" = lines,
    "neon" = text,
    "vintage" = lines,
    "delftware" = lines,
    "lichtenstein" = lines
  )
  buildings <- switch(theme,
    "original" = background,
    "light" = background,
    "dark" = background,
    "colored" = c("#8e76a4", "#a193b1", "#db9b33", "#e8c51e", "#ed6c2e"),
    "rouge" = background,
    "verde" = lines,
    "neon" = background,
    "delftware" = landuse,
    "vintage" = c("#facc87", "#f39848", "#f8c98c", "#f58762"),
    "lichtenstein" = "#f4d849"
  )
  font <- switch(theme,
    "original" = "Caveat",
    "light" = "Imbue",
    "dark" = "Imbue",
    "colored" = "Damion",
    "rouge" = "Oswald",
    "verde" = "Righteous",
    "neon" = "Neonderthaw",
    "delftware" = "Dancing Script",
    "vintage" = "Fredericka the Great",
    "lichtenstein" = "Rampart One"
  )
  face <- if (theme %in% c("original", "verde", "rouge", "neon", "delftware")) "bold" else "plain"
  neighborhood <- switch(theme,
    "original" = lines,
    "light" = lines,
    "dark" = lines,
    "colored" = "#32130f",
    "rouge" = lines,
    "verde" = "#fafafa",
    "neon" = rails,
    "delftware" = "#000000",
    "vintage" = lines,
    "lichtenstein" = lines
  )
  ruler <- switch(theme,
    "original" = lines,
    "light" = lines,
    "dark" = lines,
    "colored" = text,
    "rouge" = lines,
    "verde" = text,
    "neon" = text,
    "delftware" = text,
    "vintage" = lines,
    "lichtenstein" = lines
  )
  streetCol <- lines
  themeOptions <- list(
    "lines" = lines,
    "background" = background,
    "water" = water,
    "water.line" = waterlines,
    "landuse" = landuse,
    "text" = text,
    "rails" = rails,
    "buildings" = buildings,
    "font" = font,
    "face" = face,
    "neighborhood" = neighborhood,
    "ruler" = ruler,
    "streetCol" = streetCol,
    "theme" = theme
  )
  return(themeOptions)
}

.getStreetSize <- function(x) {
  size <- switch(x,
    "tiny" = 0.1,
    "small" = 0.4,
    "smaller" = 0.5,
    "regular" = 0.55,
    "larger" = 0.6,
    "huge" = 0.7,
    "huger" = 0.8,
    "rails" = 0.35,
    "runway" = 3
  )
  return(size)
}

.getBorderSize <- function(x) {
  size <- switch(x,
    "regular" = 0.3,
    "large" = 0.4,
    "larger" = 0.5,
    "huge" = 0.6
  )
  return(size)
}
