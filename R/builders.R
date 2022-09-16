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

.buildCity <- function(city, bbox, zoom, panel, themeOptions, border, halftone, places, cropped, borderPoints, verbose, license, ticks, shiny) {
  if (verbose) {
    # Initialize progress bar ##################################################
    progBar <- progress::progress_bar$new(format = "  :spin [:bar] :percent | Time remaining: :eta", total = ticks, clear = FALSE, show_after = 0)
    progBar$tick(0)
    progBar$message(paste0("Requesting \u00A9 OpenStreetMap features for ", city[["name"]], ", ", city[["country"]]))
  }
  # Initialize empty plot ######################################################
  int_p <- ggplot2::ggplot()
  .tick(verbose, progBar, ticks, shiny)
  #
  # Note that the following looks like a lot of unnecessary work (and it is).
  # However, this way we hopefully minimize the chance of a
  # `HTTP 504 - Gateway Timeout` error from the overpass server.
  #
  # Ocean and land features get special treatment ##############################
  query <- osmdata::osmdata_sf(q = osmdata::add_osm_feature(opq = bbox, key = "natural", value = "coastline"))
  if (!is.null(query[["osm_lines"]])) {
    motherObj <- .line2poly(obj = query[["osm_lines"]], bbox = panel)
    if (!is.null(motherObj[["sea"]])) {
      obj <- .checkAndCrop(motherObj[["sea"]][["geometry"]], cropped, border)
      int_p <- int_p + ggplot2::geom_sf(
        data = obj,
        fill = themeOptions[["water"]],
        color = themeOptions[["water.line"]],
        size = 0.3,
        inherit.aes = FALSE
      )
    }
    if (!is.null(motherObj[["land"]])) {
      obj <- .checkAndCrop(motherObj[["land"]][["geometry"]], cropped, border)
      int_p <- int_p + ggplot2::geom_sf(
        data = obj,
        fill = themeOptions[["background"]],
        color = themeOptions[["lines"]],
        size = 0.3,
        inherit.aes = FALSE
      )
    }
    if (!is.null(motherObj[["islands"]])) {
      obj <- .checkAndCrop(motherObj[["islands"]][["geometry"]], cropped, border)
      int_p <- int_p + ggplot2::geom_sf(
        data = obj,
        fill = themeOptions[["background"]],
        color = themeOptions[["lines"]],
        size = 0.3,
        inherit.aes = FALSE
      )
    }
  }
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"natural\"=\"coastline\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = themeOptions[["background"]],
    color = themeOptions[["lines"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  # Landuse ####################################################################
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"leisure\"=\"park\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = sample(themeOptions[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE),
    color = themeOptions[["lines"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"landuse\"=\"forest\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = sample(themeOptions[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE),
    color = themeOptions[["lines"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"natural\"=\"wood\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = sample(themeOptions[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE),
    color = themeOptions[["lines"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"landuse\"=\"grass\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = sample(themeOptions[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE),
    color = themeOptions[["lines"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"natural\"=\"scrub\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = sample(themeOptions[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE),
    color = themeOptions[["lines"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"natural\"=\"mud\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = sample(themeOptions[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE),
    color = themeOptions[["lines"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"natural\"=\"beach\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = sample(themeOptions[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE),
    color = themeOptions[["lines"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"landuse\"=\"meadow\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = sample(themeOptions[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE),
    color = themeOptions[["lines"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"landuse\"=\"farmland\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = sample(themeOptions[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE),
    color = themeOptions[["lines"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"natural\"=\"wood\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = sample(themeOptions[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE),
    color = themeOptions[["lines"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"landuse\"=\"cemetery\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = sample(themeOptions[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE),
    color = themeOptions[["lines"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"landuse\"=\"construction\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = sample(themeOptions[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE),
    color = themeOptions[["lines"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"amenity\"=\"parking\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = sample(themeOptions[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE),
    color = themeOptions[["lines"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"leisure\"=\"playground\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = sample(themeOptions[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE),
    color = themeOptions[["lines"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"leisure\"=\"pitch\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = sample(themeOptions[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE),
    color = themeOptions[["lines"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"leisure\"=\"dog_park\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = sample(themeOptions[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE),
    color = themeOptions[["lines"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"leisure\"=\"garden\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = sample(themeOptions[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE),
    color = themeOptions[["lines"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  # Water ######################################################################
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"waterway\"=\"river\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    fill = themeOptions[["water"]],
    color = themeOptions[["water.line"]],
    size = 0.6,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"waterway\"=\"canal\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    fill = themeOptions[["water"]],
    color = themeOptions[["water.line"]],
    size = 0.5,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"waterway\"=\"dock\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = themeOptions[["water"]],
    color = themeOptions[["water.line"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"natural\"=\"water\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = themeOptions[["water"]],
    color = themeOptions[["water.line"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  # Islands ####################################################################
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"natural\"=\"wetland\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = themeOptions[["background"]],
    color = themeOptions[["lines"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"place\"=\"islet\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = themeOptions[["background"]],
    color = themeOptions[["lines"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"man_made\"=\"pier\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  # Water lines ################################################################
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"waterway\"=\"riverbank\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["water"]],
    size = 0.4,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"waterway\"=\"stream\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["water"]],
    size = 0.4,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"waterway\"=\"ditch\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["water"]],
    size = 0.4,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  # Landuse lines ##############################################################
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"natural\"=\"coastline\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"natural\"=\"peninsula\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"place\"=\"archipelago\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  # Airports ###################################################################
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"aeroway\"=\"taxiway\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.7,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"aeroway\"=\"runway\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 3,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"railway\"=\"rail\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["rails"]],
    size = 0.35,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  # Small streets ##############################################################
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"waterway\"=\"dam\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.5,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"waterway\"=\"lock_gate\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.5,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"waterway\"=\"sluice_gate\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.5,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"waterway\"=\"bridge\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.5,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"footway\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.1,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"cycleway\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.1,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"pedestrian\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.1,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"path\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.1,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"residential\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.4,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"living_street\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.4,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"unclassified\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.4,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"service\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.4,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"construction\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.4,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  # Medium streets #############################################################
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"tertiary_link\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.55,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"tertiary\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = NA,
    color = themeOptions[["lines"]],
    size = 0.6,
    inherit.aes = FALSE
  )
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.55,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"secondary_link\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.6,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"secondary\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = NA,
    color = themeOptions[["lines"]],
    size = 0.6,
    inherit.aes = FALSE
  )
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.6,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  # Large streets ##############################################################
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"primary_link\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.7,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"primary\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = NA,
    color = themeOptions[["lines"]],
    size = 0.7,
    inherit.aes = FALSE
  )
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.7,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"trunk_link\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.7,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"trunk_loop\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.7,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"trunk\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["polygons"]],
    fill = NA,
    color = themeOptions[["lines"]],
    size = 0.7,
    inherit.aes = FALSE
  )
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.7,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"motorway_link\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.8,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  obj <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"motorway\"")
  int_p <- int_p + ggplot2::geom_sf(
    data = obj[["lines"]],
    color = themeOptions[["lines"]],
    size = 0.8,
    inherit.aes = FALSE
  )
  .tick(verbose, progBar, ticks, shiny)
  # Buildings ##################################################################
  if (zoom >= 0.5) { # Skip buildings if zoom < 0.5 as it takes immense computing time
    obj <- .getOsmFeatures(bbox, cropped, border, features = "\"building\"")
    int_p <- int_p + ggplot2::geom_sf(
      data = obj[["polygons"]],
      fill = sample(themeOptions[["buildings"]], size = length(obj[["polygons"]]), replace = TRUE),
      color = themeOptions[["lines"]],
      size = 0.3,
      inherit.aes = FALSE
    )
  }
  .tick(verbose, progBar, ticks, shiny)
  # Specify coordinate system for plot #########################################
  int_p <- int_p + ggplot2::coord_sf(xlim = c(panel[1], panel[3]), ylim = c(panel[2], panel[4]), expand = TRUE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(4, 0, 0, 0, "cm"))
  # Draw the border of the plot ################################################
  if (border != "none") {
    suppressMessages(expr = {
      int_p <- int_p + ggplot2::geom_sf(data = cropped, fill = NA, color = themeOptions[["background"]], size = 1) +
        ggplot2::geom_path(data = borderPoints, mapping = ggplot2::aes(x = x, y = y), color = themeOptions[["text"]], size = 1, inherit.aes = FALSE)
    })
  }
  # Add neighborhood names
  if (places > 0) {
    int_p <- .addPlaces(int_p, places, themeOptions, bbox, border, cropped, city)
    .tick(verbose, progBar, ticks, shiny)
  }
  # Add the city name to the plot ##############################################
  plotName <- if (themeOptions[["theme"]] %in% c("light", "dark")) paste0("\u2014", city[["name"]], "\u2014") else city[["name"]]
  p <- cowplot::ggdraw(int_p)
  if (halftone != "none") { # Halftone
    p <- .addHalftone(p, halftone)
    .tick(verbose, progBar, ticks, shiny)
  }
  p <- p + cowplot::draw_text(
    text = plotName,
    x = 0.5,
    y = 0.93,
    size = 110,
    color = themeOptions[["text"]],
    family = themeOptions[["font"]],
    fontface = themeOptions[["face"]]
  ) +
    cowplot::draw_text(
      text = city[["country"]],
      x = 0.5,
      y = 0.975,
      size = 50,
      color = themeOptions[["text"]],
      family = themeOptions[["font"]]
    ) +
    ggspatial::annotation_north_arrow(
      location = "bl",
      height = ggplot2::unit(4, "cm"),
      width = ggplot2::unit(4, "cm"),
      pad_x = ggplot2::unit(1, "cm"),
      pad_y = ggplot2::unit(1, "cm"),
      style = ggspatial::north_arrow_nautical(
        line_col = themeOptions[["text"]],
        text_size = 25,
        text_face = themeOptions[["face"]],
        text_family = themeOptions[["font"]],
        text_col = themeOptions[["text"]],
        fill = c(themeOptions[["text"]], themeOptions[["background"]])
      )
    ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = themeOptions[["background"]], color = themeOptions[["lines"]]),
      panel.background = ggplot2::element_rect(fill = themeOptions[["background"]], color = themeOptions[["background"]])
    )
  # Add the coordinates to the plot ############################################
  if (city[["lat"]] < 0) {
    lat <- paste0(format(abs(city[["lat"]]), digits = 6), "\u00B0 S")
  } else {
    lat <- paste0(format(city[["lat"]], digits = 6), "\u00B0 N")
  }
  if (city[["long"]] < 0) {
    long <- paste0(format(abs(city[["long"]]), digits = 6), "\u00B0 W")
  } else {
    long <- paste0(format(city[["long"]], digits = 6), "\u00B0 E")
  }
  p <- p + cowplot::draw_text(
    text = paste0(lat, " / ", long),
    x = 0.97,
    y = 0.03,
    size = 40,
    color = themeOptions[["text"]],
    family = themeOptions[["font"]],
    hjust = 1
  )
  # Add the OpenStreetMap licence to the plot ##################################
  if (license) {
    p <- p + cowplot::draw_text(
      text = "Data by \u00A9 OpenStreetMap contributors",
      x = 0.97,
      y = 0.01,
      size = 20,
      color = themeOptions[["text"]],
      family = themeOptions[["font"]],
      hjust = 1
    )
  }
  .tick(verbose, progBar, ticks, shiny)
  return(p)
}

.getBoundaries <- function(city, border, zoom = NULL, input = NULL) {
  defaultRadius <- 0.0225
  if (!is.null(zoom)) {
    radius <- geosphere::distm(x = c(city[["long"]], city[["lat"]]), y = c(city[["long"]], city[["lat"]] + defaultRadius * (1 / zoom)), fun = geosphere::distHaversine)
  } else {
    radius <- geosphere::distm(x = c(city[["long"]], city[["lat"]]), y = c(city[["long"]], input[["osm_bounds"]][["north"]]), fun = geosphere::distHaversine)
  }
  cropped <- data.frame(lat = city[["lat"]], long = city[["long"]]) |>
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326) |>
    sf::st_buffer(dist = radius)
  croppedBox <- lapply(sf::st_geometry(cropped), sf::st_bbox)[[1]]
  borderPoints <- .makeCircle(
    long = city[["long"]],
    lat = city[["lat"]],
    rlong = (abs(croppedBox[["xmax"]]) - abs(croppedBox[["xmin"]])) / 2 * 1.0075,
    rlat = (abs(croppedBox[["ymax"]]) - abs(croppedBox[["ymin"]])) / 2 * 1.0075
  )
  panel <- c(croppedBox[["xmin"]], croppedBox[["ymin"]], croppedBox[["xmax"]], croppedBox[["ymax"]])
  # Create the border ##########################################################
  if (border == "rhombus") {
    borderPoints <- data.frame(
      x = c(panel[1], city[["long"]], panel[3], city[["long"]], panel[1]),
      y = c(city[["lat"]], panel[2], city[["lat"]], panel[4], city[["lat"]])
    )
    cropped <- sf::st_sf(sf::st_sfc(sf::st_polygon(list(as.matrix(borderPoints)))), crs = 4326)
  } else if (border == "square") {
    borderPoints <- data.frame(
      x = c(panel[1], panel[1], panel[3], panel[3], panel[1]),
      y = c(panel[2], panel[4], panel[4], panel[2], panel[2])
    )
    cropped <- sf::st_sf(sf::st_sfc(sf::st_polygon(list(as.matrix(borderPoints)))), crs = 4326)
  } else if (border == "hexagon" | border == "octagon" | border == "decagon") {
    nsides <- switch(border,
      "hexagon" = 6,
      "octagon" = 8,
      "decagon" = 10
    )
    borderPoints <- data.frame(
      x = city[["long"]] + (abs(panel[3]) - abs(panel[1])) / 2 * cos(2 * pi * 0:nsides / nsides),
      y = city[["lat"]] + (abs(panel[4]) - abs(panel[2])) / 2 * sin(2 * pi * 0:nsides / nsides)
    )
    cropped <- sf::st_sf(sf::st_sfc(sf::st_polygon(list(as.matrix(borderPoints)))), crs = 4326)
    croppedBox <- lapply(sf::st_geometry(cropped), sf::st_bbox)[[1]]
    panel <- c(croppedBox[["xmin"]], croppedBox[["ymin"]], croppedBox[["xmax"]], croppedBox[["ymax"]])
  }
  results <- list(
    panel = panel,
    cropped = cropped,
    croppedBox = croppedBox,
    borderPoints = borderPoints,
    zoom = zoom
  )
  return(results)
}

.getOsmFeatures <- function(bbox, cropped, border, features) {
  feat <- osmdata::add_osm_features(opq = bbox, features = features)
  spl <- strsplit(features, split = "=")[[1]]
  key <- gsub(pattern = '["\"]', replacement = "", x = spl[1])
  value <- gsub(pattern = '["\"]', replacement = "", x = spl[2])
  query <- osmdata::osmdata_sf(q = feat)
  if (!is.na(value) && value == "water") {
    # Remove unwanted water polygons (not exact match)
    query[["osm_multipolygons"]] <- subset(query[["osm_multipolygons"]], query[["osm_multipolygons"]][[key]] == value)
    query[["osm_polygons"]] <- subset(query[["osm_polygons"]], query[["osm_polygons"]][[key]] == value)
  }
  multipolygons <- .checkAndCrop(query[["osm_multipolygons"]][["geometry"]], cropped, border)
  polygons <- .checkAndCrop(query[["osm_polygons"]][["geometry"]], cropped, border)
  if (length(multipolygons) > 0 && length(polygons) > 0) {
    pp <- c(multipolygons, polygons)
  } else if (length(multipolygons) == 0) {
    pp <- polygons
  } else if (length(polygons) == 0) {
    pp <- multipolygons
  } else {
    pp <- NULL
  }
  lines <- .checkAndCrop(query[["osm_lines"]][["geometry"]], cropped, border)
  result <- list()
  result[["polygons"]] <- pp
  result[["lines"]] <- if (length(lines) > 0) lines else NULL
  return(result)
}

.checkAndCrop <- function(object, cropped, border) {
  if (!is.null(object)) {
    object <- sf::st_make_valid(object)
    if (border != "none") {
      sf::st_crs(object) <- sf::st_crs(cropped)
      object <- object |> sf::st_intersection(cropped)
    }
  }
  return(object)
}

.makeCircle <- function(long, lat, rlong, rlat, npoints = 10000) {
  tt <- seq(0, 2 * pi, length.out = npoints)
  xx <- long + rlong * cos(tt)
  yy <- lat + rlat * sin(tt)
  return(data.frame(x = xx, y = yy))
}

.addPlaces <- function(int_p, places, themeOptions, bbox, border, cropped, city) {
  suppressWarnings({
    desired <- c("city", "town", "suburb", "village", "quarter", "neighbourhood", "hamlet")
    obj <- osmdata::osmdata_sf(q = osmdata::add_osm_feature(opq = bbox, key = "place", value = desired))[["osm_points"]]
    obj <- sf::st_make_valid(obj)
    if (border != "none") {
      sf::st_crs(obj) <- sf::st_crs(cropped)
      obj <- obj |> sf::st_intersection(cropped)
    }
    if (city[["name"]] %in% obj[["name"]]) {
      obj <- obj[-which(obj[["name"]] == city[["name"]]), ] # Remove original city name
    }
    if (is.null(obj[["name.en"]]) | all(is.na(obj[["name.en"]]))) {
      obj <- obj[which(!is.na(obj[["name"]])), ]
      df <- data.frame(name = obj[["name"]], place = obj[["place"]], x = unlist(lapply(obj[["geometry"]], `[[`, 1)), y = unlist(lapply(obj[["geometry"]], `[[`, 2)))
    } else {
      obj <- obj[which(!is.na(obj[["name.en"]])), ]
      df <- data.frame(name = obj[["name.en"]], place = obj[["place"]], x = unlist(lapply(obj[["geometry"]], `[[`, 1)), y = unlist(lapply(obj[["geometry"]], `[[`, 2)))
    }
    df <- df[!is.na(df[["place"]]), , drop = FALSE]
    df <- df[!duplicated(df[["name"]]), , drop = FALSE]
    if (nrow(df) > 0) {
      df <- df[order(match(df[["place"]], desired)), ]
      df <- df[1:min(nrow(df), places), ]
      df[["size"]] <- ifelse(df[["place"]] == "city", yes = 13, no = ifelse(df[["place"]] == "town", yes = 12, no = ifelse(df[["place"]] == "village", yes = 11, no = 9)))
      int_p <- int_p + shadowtext::geom_shadowtext(
        data = df,
        mapping = ggplot2::aes(x = x, y = y, label = name),
        col = themeOptions[["neighborhood"]],
        check_overlap = TRUE,
        fontface = "bold.italic",
        bg.colour = themeOptions[["background"]],
        size = df[["size"]]
      )
    }
  })
  return(int_p)
}

.addHalftone <- function(p, halftone) {
  x1 <- seq(0, 1, length = 101)
  x2 <- x1 + x1[2] / 2
  x2 <- x2[x2 > 0 & x2 < 1]
  x <- c(x1, x2)
  x <- rep(x, times = 100)
  y <- rep(0, length(x1))
  for (i in 1:199) {
    if (i %% 2 == 0) {
      y <- c(y, rep(y[length(y)] + 0.005, length(x1)))
    } else {
      y <- c(y, rep(y[length(y)] + 0.005, length(x2)))
    }
  }
  p <- p + ggplot2::geom_point(
    data = data.frame(x = x, y = y),
    mapping = ggplot2::aes(x = x, y = y),
    col = switch(halftone,
      "light" = "#ffffff",
      "dark" = "#000000"
    ),
    alpha = 0.1,
    size = 2,
    shape = 19
  )
  return(p)
}

################################################################################
# Fix for a bug in the 'osmplotr' package
# See https://github.com/ropensci/osmplotr/issues/29
# The following functions are taken over from 'osmplotr'
# Note: This is not my own work
################################################################################

.line2poly <- function(obj, bbox) {
  if (!methods::is(obj$geometry, "sfc_LINESTRING")) {
    stop("obj must be class 'sf' with fields of class 'sfc_LINESTRING'")
  }
  g <- obj$geom
  bbox <- matrix(bbox, nrow = 2)
  colnames(bbox) <- c("min", "max")
  rownames(bbox) <- c("x", "y")
  head_tail <- t(sapply(g, function(x) rownames(x)[c(1, nrow(x))]))
  m2 <- match(head_tail[, 2], head_tail[, 1])
  m1 <- match(head_tail[, 1], head_tail[, 2])
  startidx <- which(is.na(m1))
  if (nrow(head_tail) > 1 & length(startidx) >= 1) {
    linkorders <- lapply(startidx, function(x, v) .unroll(x, v), v = m2)
    linkorders <- lapply(linkorders, function(x) x[!is.na(x)])
    links <- lapply(linkorders, function(x) head_tail[x, , drop = FALSE])
    head_tail <- head_tail[-unlist(linkorders), , drop = FALSE]
    links <- lapply(links, function(x, g) .lookup_ways(x, g), g = g)
  } else {
    links <- list()
  }
  to_become_polygons <- list()
  lidx <- 1
  while (nrow(head_tail) > 0) {
    m2 <- match(head_tail[, 2], head_tail[, 1])
    if (any(!is.na(m2))) {
      l1 <- .unroll_loop(1, m2)
      to_become_polygons[[lidx]] <- head_tail[l1, ]
      lidx <- lidx + 1
      head_tail <- head_tail[-l1, ]
    } else {
      head_tail <- head_tail[-1, ]
    }
  }
  to_become_polygons <- lapply(to_become_polygons, .lookup_ways, g = g)
  to_become_polygons <- lapply(to_become_polygons, .make_sf, g = g)
  to_become_polygons <- do.call(rbind, to_become_polygons)
  bbxcorners_rh <- c("NE", "SE", "SW", "NW")
  bbxcoords <- rbind(
    c(bbox[1, 2], bbox[2, 2]),
    c(bbox[1, 2], bbox[2, 1]),
    c(bbox[1, 1], bbox[2, 1]),
    c(bbox[1, 1], bbox[2, 2])
  )
  rownames(bbxcoords) <- bbxcorners_rh
  p1 <- p2 <- NULL
  if (length(links) >= 1) {
    links <- lapply(links, function(x, bbox) .clip_one(x, bbox), bbox = bbox)
    linkpoly <- lapply(links, .make_poly, bbox = bbox, g = g)
    p1 <- lapply(linkpoly, "[[", "p1")
    p2 <- lapply(linkpoly, "[[", "p2")
  }
  res <- NULL
  if (!is.null(p1) & !is.null(p2)) {
    res <- list(sea = do.call(rbind, p1), land = do.call(
      rbind,
      p2
    ))
  }
  if (length(to_become_polygons) >= 1) {
    res$islands <- to_become_polygons
  }
  return(res)
}

.unroll <- function(firstpos, v) {
  res <- firstpos
  a <- v[firstpos]
  while (!is.na(a)) {
    res <- c(res, a)
    a <- v[a]
  }
  return(res)
}

.lookup_ways <- function(ll, g) {
  gg <- g[rownames(ll)]
  gg <- do.call(rbind, lapply(gg, as.matrix))
  rr <- duplicated(rownames(gg))
  gg <- gg[!rr, ]
  return(gg)
}

.make_sf <- function(x, g) {
  x <- list(x)
  class(x) <- c("XY", "POLYGON", "sfg")
  x <- list(x)
  attr(x, "n_empty") <- 0
  class(x) <- c("sfc_POLYGON", "sfc")
  attr(x, "precision") <- 0
  attr(x, "bbox") <- attr(g, "bbox")
  attr(x, "crs") <- attr(g, "crs")
  df <- data.frame(row.names = "1")
  df[["geometry"]] <- x
  attr(df, "sf_column") <- "geometry"
  f <- factor(rep(NA_character_, length.out = ncol(df) - 1),
    levels = c("constant", "aggregate", "identity")
  )
  names(f) <- names(df)[-ncol(df)]
  attr(df, "agr") <- f
  class(df) <- c("sf", class(df))
  return(df)
}

.make_poly <- function(out, bbox, g) {
  p1 <- p2 <- NULL
  n <- nrow(out)
  first_pt <- out[1, ]
  last_pt <- out[n, ]
  first_pt_dir <- .classify_pt_dir(first_pt, bbox)
  last_pt_dir <- .classify_pt_dir(last_pt, bbox)
  bb <- bbox
  bb["x", "min"] <- min(c(bb["x", "min"], out[, 1]))
  bb["x", "max"] <- max(c(bb["x", "max"], out[, 1]))
  bb["y", "min"] <- min(c(bb["y", "min"], out[, 2]))
  bb["y", "max"] <- max(c(bb["y", "max"], out[, 2]))
  bb21 <- bb[2, 1]
  bb12 <- bb[1, 2]
  bb22 <- bb[2, 2]
  bb11 <- bb[1, 1]
  ext_corners <- rbind(c(bb12, bb22), c(bb12, bb21), c(
    bb11,
    bb21
  ), c(bb11, bb22))
  if (last_pt_dir == first_pt_dir) {
    v_first_last <- last_pt - first_pt
    v_edge <- ext_corners[first_pt_dir, ] - ext_corners[.wrp(first_pt_dir - 1), ]
    dp <- sign(sum(v_first_last * v_edge))
    if (dp < 0) {
      cw_indx <- c(.wrp(last_pt_dir - 1), last_pt_dir)
      ccw_indx <- (last_pt_dir - 1):(last_pt_dir - 4)
      ccw_indx <- .wrp(ccw_indx)
      ccw_indx <- ccw_indx[1:which.max(ccw_indx == first_pt_dir)]
    } else {
      cw_indx <- last_pt_dir:(last_pt_dir + 4)
      cw_indx <- .wrp(cw_indx)
      cw_indx <- cw_indx[1:which.max(cw_indx == .wrp(first_pt_dir -
        1))]
      ccw_indx <- c(last_pt_dir, .wrp(last_pt_dir - 1))
    }
  } else {
    cw_indx <- last_pt_dir:(last_pt_dir + 4)
    cw_indx <- .wrp(cw_indx)
    cw_indx <- cw_indx[1:which.max(cw_indx == first_pt_dir)]
    ccw_indx <- (last_pt_dir - 1):(last_pt_dir - 4)
    ccw_indx <- .wrp(ccw_indx)
    ccw_indx <- ccw_indx[1:which.max(ccw_indx == first_pt_dir)]
  }
  p1 <- rbind(out, ext_corners[cw_indx, ], out[1, ])
  p2 <- rbind(out, ext_corners[ccw_indx, ], out[1, ])
  return(list(p1 = .make_sf(p1, g), p2 = .make_sf(p2, g)))
}

.unroll_loop <- function(firstpos, v) {
  res <- firstpos
  a <- v[firstpos]
  visted <- rep(FALSE, length(v))
  visted[firstpos] <- TRUE
  while (!visted[a]) {
    res <- c(res, a)
    visted[a] <- TRUE
    a <- v[a]
  }
  return(res)
}

.clip_one <- function(out, bbox) {
  indx <- (out[, 1] >= bbox[1, 1] & out[, 1] <= bbox[1, 2] &
    out[, 2] >= bbox[2, 1] & out[, 2] <= bbox[2, 2])
  indx <- as.logical(pmax(indx, c(indx[-1], FALSE), c(FALSE, indx[-length(indx)])))
  out[indx, ]
}

.classify_pt_dir <- function(pt, bbox) {
  directions <- 1:4
  names(directions) <- c("N", "E", "S", "W")
  compass <- c("W", "S", "E", "N")
  dim(compass) <- c(2, 2)
  lt <- pt < bbox[, "min"]
  gt <- pt > bbox[, "max"]
  td <- cbind(lt, gt)
  return(directions[compass[td]])
}

.wrp <- function(idxs) {
  (idxs - 1) %% 4 + 1
}
