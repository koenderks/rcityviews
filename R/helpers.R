# Copyright (C) 2022-2024 Koen Derks

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

.geocode <- function(name, country, method = "osm") {
  if (missing(name) || !is.character(name)) {
    stop("Please provide a valid location name as a string.")
  }

  # Use match.arg to validate the method parameter
  method <- match.arg(method, choices = c(
    "osm", "census", "arcgis", "census_simple", "geocodio",
    "mapbox", "google", "bing", "here", "tomtom", "nominatim", "tiger"
  ))

  # Handle API keys for methods that require them
  methods_with_keys <- c("google", "bing", "here", "tomtom", "mapbox", "geocodio")
  if (method %in% methods_with_keys) {
    api_key_env <- paste0(toupper(method), "_API_KEY")
    api_key <- Sys.getenv(api_key_env)
    if (api_key == "") {
      stop(paste0("API key for ", method, " is required. Please set the '", api_key_env, "' environment variable."))
    }
  }

  result <- tryCatch(
    {
      geocode_df <- tibble::tibble(address = paste0(name, " ", country)) %>%
        tidygeocoder::geocode(address, method = method, quiet = TRUE)

      if (any(is.na(geocode_df$lat)) || any(is.na(geocode_df$long))) {
        stop("Geocoding failed: Unable to find coordinates for the provided location name.")
      }

      city <- data.frame(
        name = name,
        country = country,
        lat = geocode_df$lat[1],
        long = geocode_df$long[1]
      )
    },
    error = function(e) {
      stop("Geocoding error: ", e$message)
    }
  )

  result <- tryCatch(
    {
      geocode_df <- tibble::tibble(address = paste0(name, " ", country)) %>%
        tidygeocoder::geocode(address, method = method, quiet = TRUE)

      if (any(is.na(geocode_df$lat)) || any(is.na(geocode_df$long))) {
        stop("Geocoding failed: Unable to find coordinates for the provided location name.")
      }

      city <- data.frame(
        name = name,
        country = country,
        lat = geocode_df$lat[1],
        long = geocode_df$long[1]
      )
    },
    error = function(e) {
      stop("Geocoding error: ", e$message)
    }
  )

  return(result)
}

.getCity <- function(name) {
  if (is.null(name)) {
    city <- .randomCity(NULL)
  } else {
    if (inherits(name, "rcityviewsCity")) {
      city <- name
    } else if (inherits(name, "data.frame")) {
      stopifnot("input data frame is missing 'name' column" = "name" %in% colnames(name))
      stopifnot("input data frame is missing 'country' column" = "country" %in% colnames(name))
      stopifnot("input data frame is missing 'lat' column" = "lat" %in% colnames(name))
      stopifnot("input data frame is missing 'long' column" = "long" %in% colnames(name))
      city <- name
    } else {
      dataset <- rcityviews::cities
      indexes <- which(dataset[["name"]] == name)
      index <- .resolveConflicts(name, indexes, dataset)
      if (is.null(index)) {
        city <- .geocode(
          name = name$name, country = name$country
        )
      } else {
        city <- dataset[index, ]
      }
      return(city)
    }
  }
  return(city)
}

.randomCity <- function(seed) {
  set.seed(seed)
  dataset <- rcityviews::cities
  dataset <- subset(dataset, dataset[["population"]] > 200000)
  index <- sample.int(nrow(dataset), size = 1)
  selected <- dataset[index, ]
  return(selected)
}

.resolveConflicts <- function(name, indexes, dataset) {
  index <- indexes
  if (length(indexes) == 0) {
    stop(paste0("There is no city called '", name, "' in the available data.\n Use new_city function() to geocode the location"))
  } else if (length(indexes) > 1) {
    selection <- utils::menu(
      choices = paste0(dataset[indexes, 1], ", ", dataset[indexes, 2], " | Lat: ", round(dataset[indexes, 3], 3), " | Long: ", round(dataset[indexes, 4], 3)),
      title = "More than one city matched to this name, which one to pick?"
    )
    index <- indexes[selection]
  }
  return(index)
}

.isColor <- function(x) {
  res <- try(grDevices::col2rgb(x), silent = TRUE)
  return(!inherits(res, "try-error"))
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
    ),
    "midearth" = list(
      "background" = "#b8a580",
      "water" = "#c3c9b6",
      "landuse" = "#b8a580",
      "contours" = "#53402a",
      "streets" = "#221c18",
      "rails" = "#221c18",
      "buildings" = "#53402a",
      "text" = "#221c18",
      "waterlines" = "#c3c9b6",
      "textshadow" = "#f7f3ea"
    ),
    "batik" = list(
      "background" = "#161417",
      "water" = "#214040",
      "landuse" = c("#ece3d9", "#9e5426", "#5d473c", "#C0b28a"),
      "contours" = "#1d1d23",
      "streets" = "#d7c5b8",
      "rails" = "#d7c5b8",
      "buildings" = c("#ece3d9", "#9e5426", "#5d473c", "#c0b28a"),
      "text" = "#d7c5b8",
      "waterlines" = "#214040"
    ),
    "vice" = list(
      "background" = "#ffffff",
      "water" = "#a3bff4",
      "landuse" = "#6ece92",
      "contours" = "#000000",
      "streets" = "#e282af",
      "rails" = "#e282af",
      "buildings" = "#fff01f",
      "text" = "#ffffff",
      "waterlines" = "#a3bff4",
      "textshadow" = "#e282af"
    )
  )
  font <- switch(theme,
    "vintage" = list(
      "family" = "Fredericka the Great",
      "face" = "plain",
      "scale" = 1
    ),
    "modern" = list(
      "family" = "Imbue",
      "face" = "plain",
      "scale" = 1
    ),
    "bright" = list(
      "family" = "Damion",
      "face" = "plain",
      "scale" = 1
    ),
    "delftware" = list(
      "family" = "Dancing Script",
      "face" = "bold",
      "scale" = 1
    ),
    "comic" = list(
      "family" = "Rampart One",
      "face" = "plain",
      "scale" = 1
    ),
    "rouge" = list(
      "family" = "Oswald",
      "face" = "bold",
      "scale" = 1
    ),
    "original" = list(
      "family" = "Caveat",
      "face" = "bold",
      "scale" = 1
    ),
    "midearth" = list(
      "family" = "American Uncial Regular",
      "face" = "plain",
      "scale" = 1
    ),
    "batik" = list(
      "family" = "Walter Turncoat",
      "face" = "plain",
      "scale" = 1
    ),
    "vice" = list(
      "family" = "Rage",
      "face" = "bold",
      "scale" = 1
    )
  )
  size <- list()
  size[["borders"]] <- list(
    "contours" = 0.15,
    "water" = 0.4,
    "canal" = 0.5,
    "river" = 0.6
  )
  size[["streets"]] <- list(
    "path" = 0.2,
    "residential" = 0.3,
    "structure" = 0.35,
    "tertiary" = 0.4,
    "secondary" = 0.5,
    "primary" = 0.6,
    "motorway" = 0.8,
    "rails" = 0.65,
    "runway" = 3
  )
  themeOptions <- list(
    "colors" = colors,
    "font" = font,
    "size" = size
  )
  return(themeOptions)
}

.checkThemeOptions <- function(themeOptions) {
  # Check for NA's in theme
  stopifnot("'theme' should not contain NA values" = !any(sapply(themeOptions, anyNA)))
  # Checks for colors sublist
  stopifnot("'theme' should contain a list element named 'colors'" = !is.null(themeOptions[["colors"]]))
  stopifnot("the 'color' list in 'theme' should contain an entry named 'background'" = !is.null(themeOptions[["colors"]][["background"]]))
  stopifnot("the 'color' list in 'theme' should contain an entry named 'water'" = !is.null(themeOptions[["colors"]][["water"]]))
  stopifnot("the 'color' list in 'theme' should contain an entry named 'landuse'" = !is.null(themeOptions[["colors"]][["landuse"]]))
  stopifnot("the 'color' list in 'theme' should contain an entry named 'contours'" = !is.null(themeOptions[["colors"]][["contours"]]))
  stopifnot("the 'color' list in 'theme' should contain an entry named 'streets'" = !is.null(themeOptions[["colors"]][["streets"]]))
  stopifnot("the 'color' list in 'theme' should contain an entry named 'rails'" = !is.null(themeOptions[["colors"]][["rails"]]))
  stopifnot("the 'color' list in 'theme' should contain an entry named 'buildings'" = !is.null(themeOptions[["colors"]][["buildings"]]))
  stopifnot("the 'color' list in 'theme' should contain an entry named 'text'" = !is.null(themeOptions[["colors"]][["text"]]))
  stopifnot("the 'color' list in 'theme' should contain an entry named 'waterlines'" = !is.null(themeOptions[["colors"]][["waterlines"]]))
  stopifnot("the 'colors' list in 'theme' should contain all valid color representations" = all(sapply(themeOptions[["colors"]], .isColor)))
  # Checks for font sublist
  stopifnot("'theme' should contain a list element named 'font'" = !is.null(themeOptions[["font"]]))
  stopifnot("the 'font' list in 'theme' should contain an entry named 'family'" = !is.null(themeOptions[["font"]][["family"]]))
  stopifnot("the 'font' list in 'theme' should contain an entry named 'face'" = !is.null(themeOptions[["font"]][["family"]]))
  stopifnot("the 'font' list in 'theme' should contain an entry named 'scale'" = !is.null(themeOptions[["font"]][["family"]]))
  stopifnot("the 'font' list in 'theme' should contain an entry named 'append'" = !is.null(themeOptions[["font"]][["family"]]))
  # Checks for size sublist
  stopifnot("'theme' should contain a list element named 'size'" = !is.null(themeOptions[["size"]]))
  # Checks for borders sublist
  stopifnot("the 'size' list in 'theme' should contain a list named 'borders'" = !is.null(themeOptions[["size"]][["borders"]]))
  stopifnot("the 'borders' list in the 'size' list in 'theme' should contain an entry named 'contours'" = !is.null(themeOptions[["size"]][["borders"]][["contours"]]))
  stopifnot("the 'borders' list in the 'size' list in 'theme' should contain an entry named 'water'" = !is.null(themeOptions[["size"]][["borders"]][["water"]]))
  stopifnot("the 'borders' list in the 'size' list in 'theme' should contain an entry named 'canal'" = !is.null(themeOptions[["size"]][["borders"]][["canal"]]))
  stopifnot("the 'borders' list in the 'size' list in 'theme' should contain an entry named 'river'" = !is.null(themeOptions[["size"]][["borders"]][["river"]]))
  stopifnot("the 'borders' list in the 'size' list in 'theme' should contain all numeric values" = all(sapply(themeOptions[["size"]][["borders"]], is.numeric)))
  # Checks for streets sublist
  stopifnot("the 'size' list in 'theme' should contain a list named 'streets'" = !is.null(themeOptions[["size"]][["streets"]]))
  stopifnot("the 'streets' list in the 'size' list in 'theme' should contain an entry named 'path'" = !is.null(themeOptions[["size"]][["streets"]][["path"]]))
  stopifnot("the 'streets' list in the 'size' list in 'theme' should contain an entry named 'residential'" = !is.null(themeOptions[["size"]][["streets"]][["residential"]]))
  stopifnot("the 'streets' list in the 'size' list in 'theme' should contain an entry named 'structure'" = !is.null(themeOptions[["size"]][["streets"]][["structure"]]))
  stopifnot("the 'streets' list in the 'size' list in 'theme' should contain an entry named 'tertiary'" = !is.null(themeOptions[["size"]][["streets"]][["tertiary"]]))
  stopifnot("the 'streets' list in the 'size' list in 'theme' should contain an entry named 'secondary'" = !is.null(themeOptions[["size"]][["streets"]][["secondary"]]))
  stopifnot("the 'streets' list in the 'size' list in 'theme' should contain an entry named 'primary'" = !is.null(themeOptions[["size"]][["streets"]][["primary"]]))
  stopifnot("the 'streets' list in the 'size' list in 'theme' should contain an entry named 'motorway'" = !is.null(themeOptions[["size"]][["streets"]][["motorway"]]))
  stopifnot("the 'streets' list in the 'size' list in 'theme' should contain an entry named 'rails'" = !is.null(themeOptions[["size"]][["streets"]][["rails"]]))
  stopifnot("the 'streets' list in the 'size' list in 'theme' should contain an entry named 'runway'" = !is.null(themeOptions[["size"]][["streets"]][["runway"]]))
  stopifnot("the 'streets' list in the 'size' list in 'theme' should contain all numeric values" = all(sapply(themeOptions[["size"]][["streets"]], is.numeric)))
}
