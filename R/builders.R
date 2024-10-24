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

.nonMemoiseRequestData <- function(city, bbox, zoom, panel, border, cropped, verbose, shiny) {
  ticks <- 61
  if (verbose) {
    # Initialize progress bar ##################################################
    progBar <- progress::progress_bar$new(format = "  :spin [:bar] :percent | Time remaining: :eta", total = ticks, clear = FALSE, show_after = 0)
    progBar$tick(0)
    progBar$message(paste0("Retrieving \u00A9 OpenStreetMap features for ", city[["name"]], ", ", city[["country"]]))
  }
  # Initialize empty list ######################################################
  imgData <- list()
  .tick(verbose, progBar, ticks, shiny)
  # Ocean and land features get special treatment ##############################
  query <- osmdata::osmdata_sf(q = osmdata::add_osm_feature(opq = bbox, key = "natural", value = "coastline"))
  if (!is.null(query[["osm_lines"]])) {
    motherObj <- .line2poly(obj = query[["osm_lines"]], bbox = panel)
    if (!is.null(motherObj[["sea"]])) {
      imgData[["layer1"]] <- .checkAndCrop(motherObj[["sea"]][["geometry"]], cropped, border)
    }
    if (!is.null(motherObj[["land"]])) {
      imgData[["layer2"]] <- .checkAndCrop(motherObj[["land"]][["geometry"]], cropped, border)
    }
    if (!is.null(motherObj[["islands"]])) {
      imgData[["layer3"]] <- .checkAndCrop(motherObj[["islands"]][["geometry"]], cropped, border)
    }
  }
  imgData[["layer4"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"natural\"=\"coastline\"")
  .tick(verbose, progBar, ticks, shiny)
  # Landuse ####################################################################
  imgData[["layer5"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"leisure\"=\"park\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer6"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"landuse\"=\"forest\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer7"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"natural\"=\"wood\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer8"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"landuse\"=\"grass\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer9"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"natural\"=\"scrub\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer10"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"natural\"=\"mud\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer11"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"natural\"=\"beach\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer12"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"natural\"=\"heath\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer13"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"landuse\"=\"meadow\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer14"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"landuse\"=\"farmland\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer15"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"natural\"=\"wood\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer16"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"landuse\"=\"cemetery\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer17"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"landuse\"=\"construction\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer18"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"amenity\"=\"parking\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer19"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"leisure\"=\"playground\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer20"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"leisure\"=\"pitch\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer21"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"leisure\"=\"dog_park\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer22"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"leisure\"=\"garden\"")
  .tick(verbose, progBar, ticks, shiny)
  # Water ######################################################################
  imgData[["layer23"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"waterway\"=\"river\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer24"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"waterway\"=\"canal\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer25"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"waterway\"=\"dock\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer26"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"natural\"=\"water\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer27"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"natural\"=\"wetland\"")
  .tick(verbose, progBar, ticks, shiny)
  # Islands ####################################################################
  imgData[["layer28"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"place\"=\"islet\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer29"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"man_made\"=\"pier\"")
  .tick(verbose, progBar, ticks, shiny)
  # Water lines ################################################################
  imgData[["layer30"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"waterway\"=\"riverbank\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer31"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"waterway\"=\"stream\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer32"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"waterway\"=\"ditch\"")
  .tick(verbose, progBar, ticks, shiny)
  # Landuse lines ##############################################################
  imgData[["layer33"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"natural\"=\"coastline\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer34"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"natural\"=\"peninsula\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer35"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"place\"=\"archipelago\"")
  .tick(verbose, progBar, ticks, shiny)
  # Airports ###################################################################
  imgData[["layer36"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"aeroway\"=\"taxiway\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer37"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"aeroway\"=\"runway\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer38"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"railway\"=\"rail\"")
  .tick(verbose, progBar, ticks, shiny)
  # Small streets ##############################################################
  imgData[["layer39"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"waterway\"=\"dam\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer40"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"waterway\"=\"lock_gate\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer41"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"waterway\"=\"sluice_gate\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer42"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"waterway\"=\"bridge\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer43"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"footway\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer44"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"cycleway\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer45"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"pedestrian\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer46"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"path\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer47"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"residential\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer48"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"living_street\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer49"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"unclassified\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer50"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"service\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer51"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"construction\"")
  .tick(verbose, progBar, ticks, shiny)
  # Medium streets #############################################################
  imgData[["layer52"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"tertiary_link\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer53"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"tertiary\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer54"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"secondary_link\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer55"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"secondary\"")
  .tick(verbose, progBar, ticks, shiny)
  # Large streets ##############################################################
  imgData[["layer56"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"primary_link\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer57"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"primary\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer58"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"trunk_link\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer59"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"trunk_loop\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer60"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"trunk\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer61"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"motorway_link\"")
  .tick(verbose, progBar, ticks, shiny)
  imgData[["layer62"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"highway\"=\"motorway\"")
  .tick(verbose, progBar, ticks, shiny)
  # Buildings ##################################################################
  imgData[["layer63"]] <- .getOsmFeatures(bbox, cropped, border, features = "\"building\"")
  .tick(verbose, progBar, ticks, shiny)
  return(imgData)
}

.buildCity <- function(imgData, city, bbox, zoom, panel, themeOptions, border, halftone, legend, places, cropped, borderPoints, license) {
  label <- NULL
  # Scale the streets with the zoom
  themeOptions[["size"]][["streets"]] <- lapply(themeOptions[["size"]][["streets"]], FUN = "*", zoom)
  themeOptions[["size"]][["borders"]] <- lapply(themeOptions[["size"]][["borders"]], FUN = "*", zoom)
  # Initialize empty plot ######################################################
  int_p <- ggplot2::ggplot()
  # Ocean and land features ####################################################
  int_p <- int_p +
    ggplot2::geom_sf(
      data = imgData[["layer1"]],
      fill = themeOptions[["colors"]][["water"]],
      color = themeOptions[["colors"]][["waterlines"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer2"]],
      fill = themeOptions[["colors"]][["background"]],
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer3"]],
      fill = themeOptions[["colors"]][["background"]],
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer4"]][["polygons"]],
      fill = themeOptions[["colors"]][["background"]],
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    )
  # Landuse ####################################################################
  int_p <- int_p +
    ggplot2::geom_sf(
      data = imgData[["layer5"]][["polygons"]],
      fill = sample(themeOptions[["colors"]][["landuse"]], size = length(imgData[["layer5"]][["polygons"]]), replace = TRUE),
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer6"]][["polygons"]],
      fill = sample(themeOptions[["colors"]][["landuse"]], size = length(imgData[["layer6"]][["polygons"]]), replace = TRUE),
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer7"]][["polygons"]],
      fill = sample(themeOptions[["colors"]][["landuse"]], size = length(imgData[["layer7"]][["polygons"]]), replace = TRUE),
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer8"]][["polygons"]],
      fill = sample(themeOptions[["colors"]][["landuse"]], size = length(imgData[["layer8"]][["polygons"]]), replace = TRUE),
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer9"]][["polygons"]],
      fill = sample(themeOptions[["colors"]][["landuse"]], size = length(imgData[["layer9"]][["polygons"]]), replace = TRUE),
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer10"]][["polygons"]],
      fill = sample(themeOptions[["colors"]][["landuse"]], size = length(imgData[["layer10"]][["polygons"]]), replace = TRUE),
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer11"]][["polygons"]],
      fill = sample(themeOptions[["colors"]][["landuse"]], size = length(imgData[["layer11"]][["polygons"]]), replace = TRUE),
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer12"]][["polygons"]],
      fill = sample(themeOptions[["colors"]][["landuse"]], size = length(imgData[["layer12"]][["polygons"]]), replace = TRUE),
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer13"]][["polygons"]],
      fill = sample(themeOptions[["colors"]][["landuse"]], size = length(imgData[["layer13"]][["polygons"]]), replace = TRUE),
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer14"]][["polygons"]],
      fill = sample(themeOptions[["colors"]][["landuse"]], size = length(imgData[["layer14"]][["polygons"]]), replace = TRUE),
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer15"]][["polygons"]],
      fill = sample(themeOptions[["colors"]][["landuse"]], size = length(imgData[["layer15"]][["polygons"]]), replace = TRUE),
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer16"]][["polygons"]],
      fill = sample(themeOptions[["colors"]][["landuse"]], size = length(imgData[["layer16"]][["polygons"]]), replace = TRUE),
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer17"]][["polygons"]],
      fill = sample(themeOptions[["colors"]][["landuse"]], size = length(imgData[["layer17"]][["polygons"]]), replace = TRUE),
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer18"]][["polygons"]],
      fill = sample(themeOptions[["colors"]][["landuse"]], size = length(imgData[["layer18"]][["polygons"]]), replace = TRUE),
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer19"]][["polygons"]],
      fill = sample(themeOptions[["colors"]][["landuse"]], size = length(imgData[["layer19"]][["polygons"]]), replace = TRUE),
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer20"]][["polygons"]],
      fill = sample(themeOptions[["colors"]][["landuse"]], size = length(imgData[["layer20"]][["polygons"]]), replace = TRUE),
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer21"]][["polygons"]],
      fill = sample(themeOptions[["colors"]][["landuse"]], size = length(imgData[["layer21"]][["polygons"]]), replace = TRUE),
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer22"]][["polygons"]],
      fill = sample(themeOptions[["colors"]][["landuse"]], size = length(imgData[["layer22"]][["polygons"]]), replace = TRUE),
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    )
  # Water ######################################################################
  int_p <- int_p +
    ggplot2::geom_sf(
      data = imgData[["layer23"]][["lines"]],
      fill = themeOptions[["colors"]][["water"]],
      color = themeOptions[["colors"]][["waterlines"]],
      linewidth = themeOptions[["size"]][["borders"]][["river"]],
      lineend = "round",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer24"]][["lines"]],
      fill = themeOptions[["colors"]][["water"]],
      color = themeOptions[["colors"]][["waterlines"]],
      linewidth = themeOptions[["size"]][["borders"]][["canal"]],
      lineend = "round",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer25"]][["polygons"]],
      fill = themeOptions[["colors"]][["water"]],
      color = themeOptions[["colors"]][["waterlines"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer26"]][["polygons"]],
      fill = themeOptions[["colors"]][["water"]],
      color = themeOptions[["colors"]][["waterlines"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer27"]][["polygons"]],
      fill = themeOptions[["colors"]][["water"]],
      color = themeOptions[["colors"]][["waterlines"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    )
  # Islands ####################################################################
  int_p <- int_p +
    ggplot2::geom_sf(
      data = imgData[["layer28"]][["polygons"]],
      fill = themeOptions[["colors"]][["background"]],
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer29"]][["lines"]],
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["streets"]][["path"]],
      lineend = "round",
      inherit.aes = FALSE
    )
  # Water lines ################################################################
  int_p <- int_p +
    ggplot2::geom_sf(
      data = imgData[["layer30"]][["lines"]],
      color = themeOptions[["colors"]][["water"]],
      linewidth = themeOptions[["size"]][["borders"]][["water"]],
      lineend = "round",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer31"]][["lines"]],
      color = themeOptions[["colors"]][["water"]],
      linewidth = themeOptions[["size"]][["borders"]][["water"]],
      lineend = "round",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer32"]][["lines"]],
      color = themeOptions[["colors"]][["water"]],
      linewidth = themeOptions[["size"]][["borders"]][["water"]],
      lineend = "round",
      inherit.aes = FALSE
    )
  # Landuse lines ##############################################################
  int_p <- int_p +
    ggplot2::geom_sf(
      data = imgData[["layer33"]][["lines"]],
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      lineend = "round",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer34"]][["lines"]],
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      lineend = "round",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer35"]][["lines"]],
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      lineend = "round",
      inherit.aes = FALSE
    )
  # Airports ###################################################################
  int_p <- int_p +
    ggplot2::geom_sf(
      data = imgData[["layer36"]][["lines"]],
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["streets"]][["primary"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer37"]][["lines"]],
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["streets"]][["runway"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer38"]][["lines"]],
      color = themeOptions[["colors"]][["rails"]][1],
      linewidth = themeOptions[["size"]][["streets"]][["rails"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer38"]][["lines"]],
      color = themeOptions[["colors"]][["rails"]][length(themeOptions[["colors"]][["rails"]])],
      linewidth = themeOptions[["size"]][["streets"]][["rails"]],
      linetype = "dashed",
      inherit.aes = FALSE
    )
  # Small streets ##############################################################

  int_p <- int_p +
    ggplot2::geom_sf(
      data = imgData[["layer39"]][["lines"]],
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["structure"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer40"]][["lines"]],
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["structure"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer41"]][["lines"]],
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["structure"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer42"]][["lines"]],
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["structure"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer43"]][["lines"]],
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["path"]],
      lineend = "round",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer44"]][["lines"]],
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["path"]],
      lineend = "round",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer45"]][["lines"]],
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["path"]],
      lineend = "round",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer46"]][["lines"]],
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["path"]],
      lineend = "round",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer47"]][["lines"]],
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["residential"]],
      lineend = "round",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer48"]][["lines"]],
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["residential"]],
      lineend = "round",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer49"]][["lines"]],
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["residential"]],
      lineend = "round",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer50"]][["lines"]],
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["residential"]],
      lineend = "round",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer51"]][["lines"]],
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["residential"]],
      lineend = "round",
      inherit.aes = FALSE
    )
  # Medium streets #############################################################
  int_p <- int_p +
    ggplot2::geom_sf(
      data = imgData[["layer52"]][["polygons"]],
      fill = NA,
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["tertiary"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer52"]][["lines"]],
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["tertiary"]],
      lineend = "round",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer53"]][["polygons"]],
      fill = NA,
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["tertiary"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer53"]][["lines"]],
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["tertiary"]],
      lineend = "round",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer54"]][["polygons"]],
      fill = NA,
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["secondary"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer54"]][["lines"]],
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["secondary"]],
      lineend = "round",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer55"]][["polygons"]],
      fill = NA,
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["secondary"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer55"]][["lines"]],
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["secondary"]],
      lineend = "round",
      inherit.aes = FALSE
    )
  # Large streets ##############################################################
  int_p <- int_p +
    ggplot2::geom_sf(
      data = imgData[["layer56"]][["polygons"]],
      fill = NA,
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["primary"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer56"]][["lines"]],
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["primary"]],
      lineend = "round",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer57"]][["polygons"]],
      fill = NA,
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["primary"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer57"]][["lines"]],
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["primary"]],
      lineend = "round",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer58"]][["polygons"]],
      fill = NA,
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["primary"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer58"]][["lines"]],
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["primary"]],
      lineend = "round",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer59"]][["polygons"]],
      fill = NA,
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["primary"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer59"]][["lines"]],
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["primary"]],
      lineend = "round",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer60"]][["polygons"]],
      fill = NA,
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["primary"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer60"]][["lines"]],
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["primary"]],
      lineend = "round",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer61"]][["polygons"]],
      fill = NA,
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["motorway"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer61"]][["lines"]],
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["motorway"]],
      lineend = "round",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer62"]][["polygons"]],
      fill = NA,
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["motorway"]],
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = imgData[["layer62"]][["lines"]],
      color = themeOptions[["colors"]][["streets"]],
      linewidth = themeOptions[["size"]][["streets"]][["motorway"]],
      lineend = "round",
      inherit.aes = FALSE
    )
  # Buildings ##################################################################
  int_p <- int_p +
    ggplot2::geom_sf(
      data = imgData[["layer63"]][["polygons"]],
      fill = sample(themeOptions[["colors"]][["buildings"]], size = length(imgData[["layer63"]][["polygons"]]), replace = TRUE),
      color = themeOptions[["colors"]][["contours"]],
      linewidth = themeOptions[["size"]][["borders"]][["contours"]],
      inherit.aes = FALSE
    )
  # Specify coordinate system for plot #########################################
  int_p <- int_p + ggplot2::coord_sf(xlim = c(panel[1], panel[3]), ylim = c(panel[2], panel[4]), expand = (border != "none")) +
    ggplot2::theme_void()
  if (border == "none" || border == "bbox") {
    int_p <- int_p + ggplot2::theme(plot.margin = ggplot2::margin(-1, -1, -1, -1, "cm"))
  } else if (border == "square") {
    int_p <- int_p + ggplot2::theme(plot.margin = ggplot2::margin(3.5, 0, 0.5, 0, "cm"))
  } else {
    int_p <- int_p + ggplot2::theme(plot.margin = ggplot2::margin(4, 0, 0, 0, "cm"))
  }
  # Draw the border of the plot ################################################
  if (border != "none") {
    suppressMessages(expr = {
      int_p <- int_p + ggplot2::geom_sf(data = cropped, fill = NA, color = themeOptions[["colors"]][["background"]], size = 1)
      if (border != "bbox") {
        int_p <- int_p + ggplot2::geom_path(data = borderPoints, mapping = ggplot2::aes(x = x, y = y), color = themeOptions[["colors"]][["text"]], size = if (border == "square") 5 else min(3, max(1, 1 * zoom)), inherit.aes = FALSE, lineend = "round")
      }
    })
  }
  # Add names of places
  if (places > 0) {
    int_p <- .addPlaces(int_p, places, themeOptions, bbox, border, cropped, city)
  }
  # Draw the plot
  p <- cowplot::ggdraw(int_p)
  p <- p + ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = themeOptions[["colors"]][["background"]], color = themeOptions[["colors"]][["contours"]]),
    panel.background = ggplot2::element_rect(fill = themeOptions[["colors"]][["background"]], color = themeOptions[["colors"]][["background"]])
  )
  # Add halftone
  if (!is.null(halftone)) {
    p <- .addHalftone(p, halftone)
  }
  # Add the city name to the plot ##############################################
  plotName <- if (is.null(themeOptions[["font"]][["append"]])) city[["name"]] else paste0(themeOptions[["font"]][["append"]], city[["name"]], themeOptions[["font"]][["append"]])
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
  p <- p + shadowtext::geom_shadowtext(
    data = data.frame(x = 0.5, y = c(0.93, 0.975), label = c(plotName, city[["country"]])),
    mapping = ggplot2::aes(x = x, y = y, label = label),
    size = c(themeOptions[["font"]][["scale"]] * 30, themeOptions[["font"]][["scale"]] * 20),
    color = themeOptions[["colors"]][["text"]],
    fontface = themeOptions[["font"]][["face"]],
    family = themeOptions[["font"]][["family"]],
    bg.colour = if (!is.null(themeOptions[["colors"]][["textshadow"]])) themeOptions[["colors"]][["textshadow"]] else themeOptions[["colors"]][["background"]]
  ) + shadowtext::geom_shadowtext(
    data = data.frame(x = 0.97, y = 0.03, label = paste0(lat, " / ", long)),
    mapping = ggplot2::aes(x = x, y = y, label = label),
    size = themeOptions[["font"]][["scale"]] * 15,
    color = themeOptions[["colors"]][["text"]],
    family = themeOptions[["font"]][["family"]],
    bg.colour = if (!is.null(themeOptions[["colors"]][["textshadow"]])) themeOptions[["colors"]][["textshadow"]] else themeOptions[["colors"]][["background"]],
    hjust = 1
  )
  if (legend) {
    p <- .addLegend(p, bbox, themeOptions)
  }
  # Add the OpenStreetMap licence to the plot ##################################
  if (license) {
    p <- p + shadowtext::geom_shadowtext(
      data = data.frame(x = 0.97, y = 0.01, label = "Data by \u00A9 OpenStreetMap contributors"),
      mapping = ggplot2::aes(x = x, y = y, label = label),
      size = themeOptions[["font"]][["scale"]] * 9,
      color = themeOptions[["colors"]][["text"]],
      family = themeOptions[["font"]][["family"]],
      bg.colour = if (!is.null(themeOptions[["colors"]][["textshadow"]])) themeOptions[["colors"]][["textshadow"]] else themeOptions[["colors"]][["background"]],
      hjust = 1
    )
  }
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
    # sf::st_buffer(dist = radius) |> # see https://github.com/r-spatial/sf/issues/1692
    s2::s2_buffer_cells(distance = radius, max_cells = 5000) |>
    sf::st_as_sf()
  croppedBox <- lapply(sf::st_geometry(cropped), sf::st_bbox)[[1]]
  borderPoints <- .makeCircle(
    long = city[["long"]],
    lat = city[["lat"]],
    rlong = (abs(croppedBox[["xmax"]]) - abs(croppedBox[["xmin"]])) / 2 * 0.9975,
    rlat = (abs(croppedBox[["ymax"]]) - abs(croppedBox[["ymin"]])) / 2 * 0.9975
  )
  panel <- c(croppedBox[["xmin"]], croppedBox[["ymin"]], croppedBox[["xmax"]], croppedBox[["ymax"]])
  if (border == "bbox") {
    p <- try({
      borderPoints <- osmdata::getbb(place_name = city[["name"]], format_out = "polygon")
      if (is.list(borderPoints)) {
        borderPoints <- borderPoints[[1]]
      }
    })
    if (inherits(p, "try-error")) {
      border <- "none"
      message(paste0("using 'border = none' since Nominatim cannot find a bounding box for ", city[["name"]], ", please check https://nominatim.openstreetmap.org/ui/search.html"))
    } else {
      cropped <- sf::st_sf(sf::st_sfc(sf::st_polygon(list(as.matrix(borderPoints)))), crs = 4326)
      croppedBox <- lapply(sf::st_geometry(cropped), sf::st_bbox)[[1]]
      panel <- c(croppedBox[["xmin"]], croppedBox[["ymin"]], croppedBox[["xmax"]], croppedBox[["ymax"]])
      zoom <- (defaultRadius / ((croppedBox[["xmax"]] - croppedBox[["xmin"]]) / 2))
    }
  }
  if (border == "none") {
    borderPoints <- data.frame(
      x = c(panel[1], panel[1], panel[3], panel[3], panel[1]),
      y = c(panel[2], panel[4], panel[4], panel[2], panel[2])
    )
    cropped <- sf::st_sf(sf::st_sfc(sf::st_polygon(list(as.matrix(borderPoints)))), crs = 4326)
    croppedBox <- lapply(sf::st_geometry(cropped), sf::st_bbox)[[1]]
  }
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
  } else if (border == "hexagon" || border == "octagon" || border == "decagon") {
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
  suppressWarnings({ # suppress: Feature keys clash with id or metadata columns and will be renamed by appending `.n`
    query <- osmdata::osmdata_sf(q = feat)
  })
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
    sf::st_crs(object) <- sf::st_crs(cropped)
    p <- try(
      {
        object <- object |> sf::st_intersection(cropped)
      },
      silent = TRUE
    )
    if (inherits(p, "try-error")) {
      suppressMessages({
        sf::sf_use_s2(FALSE)
        object <- sf::st_make_valid(object)
        sf::st_crs(object) <- sf::st_crs(cropped)
        object <- object |> sf::st_intersection(cropped)
        sf::sf_use_s2(TRUE)
      })
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
    if (is.null(obj[["name.en"]]) || all(is.na(obj[["name.en"]]))) {
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
      df <- df[seq_len(min(nrow(df), places)), ]
      df[["size"]] <- ifelse(df[["place"]] == "city", yes = 13 * themeOptions[["font"]][["scale"]], no = ifelse(df[["place"]] == "town", yes = 12 * themeOptions[["font"]][["scale"]], no = ifelse(df[["place"]] == "village", yes = 11 * themeOptions[["font"]][["scale"]], no = 9 * themeOptions[["font"]][["scale"]])))
      int_p <- int_p + shadowtext::geom_shadowtext(
        data = df,
        mapping = ggplot2::aes(x = x, y = y, label = name),
        check_overlap = TRUE,
        color = themeOptions[["colors"]][["text"]],
        fontface = "bold.italic",
        family = themeOptions[["font"]][["family"]],
        bg.colour = if (!is.null(themeOptions[["colors"]][["textshadow"]])) themeOptions[["colors"]][["textshadow"]] else themeOptions[["colors"]][["background"]],
        size = df[["size"]]
      )
    }
  })
  return(int_p)
}

.addLegend <- function(p, bbox, themeOptions) {
  bb <- as.numeric(strsplit(bbox[["bbox"]], split = ",")[[1]])
  width <- geosphere::distHaversine(bb[c(2, 1)], bb[c(4, 1)])
  x <- pretty(seq(0, round(width / 5), length = 10), n = 4, min.n = 3, high.u.bias = 3)
  barLength <- max(x)
  x <- x / width + .01
  l <- length(x)
  barMeasure <- "m"
  if (barLength > 1000) {
    barLength <- barLength / 1000
    barMeasure <- "km"
  }
  p <- p + ggspatial::annotation_north_arrow(
    location = "bl",
    height = ggplot2::unit(4, "cm"),
    width = ggplot2::unit(4, "cm"),
    pad_x = ggplot2::unit(1, "cm"),
    pad_y = ggplot2::unit(1, "cm"),
    style = ggspatial::north_arrow_nautical(
      line_col = themeOptions[["colors"]][["text"]],
      text_size = themeOptions[["font"]][["scale"]] * 25,
      text_face = themeOptions[["font"]][["face"]],
      text_family = themeOptions[["font"]][["family"]],
      text_col = themeOptions[["colors"]][["text"]],
      fill = c(themeOptions[["colors"]][["text"]], themeOptions[["colors"]][["background"]])
    )
  ) +
    ggplot2::annotate(
      geom = "rect",
      xmin = x[-l],
      xmax = x[-1],
      ymin = 0.007,
      ymax = 0.013,
      fill = rep(c(themeOptions[["colors"]][["background"]], themeOptions[["colors"]][["text"]]), length.out = l - 1),
      col = themeOptions[["colors"]][["text"]]
    ) +
    cowplot::draw_text(
      text = paste0(barLength, barMeasure),
      x = x[l] + .01,
      y = 0.011,
      size = themeOptions[["font"]][["scale"]] * 30,
      color = themeOptions[["colors"]][["text"]],
      family = themeOptions[["font"]][["family"]],
      hjust = 0
    )
  return(p)
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
    col = halftone,
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
# Note: I did not make these functions (except the fixes)
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
  if (nrow(head_tail) > 1 && length(startidx) >= 1) {
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
  if (!is.null(p1) && !is.null(p2)) {
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
      ccw_indx <- ccw_indx[seq_len(which.max(ccw_indx == first_pt_dir))]
    } else {
      cw_indx <- last_pt_dir:(last_pt_dir + 4)
      cw_indx <- .wrp(cw_indx)
      cw_indx <- cw_indx[seq_len(which.max(cw_indx == .wrp(first_pt_dir - 1)))]
      ccw_indx <- c(last_pt_dir, .wrp(last_pt_dir - 1))
    }
  } else {
    cw_indx <- last_pt_dir:(last_pt_dir + 4)
    cw_indx <- .wrp(cw_indx)
    cw_indx <- cw_indx[seq_len(which.max(cw_indx == first_pt_dir))]
    ccw_indx <- (last_pt_dir - 1):(last_pt_dir - 4)
    ccw_indx <- .wrp(ccw_indx)
    ccw_indx <- ccw_indx[seq_len(which.max(ccw_indx == first_pt_dir))]
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
  indx <- (out[, 1] >= bbox[1, 1] & out[, 1] <= bbox[1, 2] & out[, 2] >= bbox[2, 1] & out[, 2] <= bbox[2, 2])
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
