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

#' Create an Aerial City View
#'
#' @description Create an aerial city view.
#'
#'
#' @usage cityview(name, zoom = 1,
#'          theme = c("original", "light", "dark", "colored",
#'                    "rouge", "verde", "neon", "lichtenstein"),
#'          border = c("none", "circle", "rhombus", "square",
#'                     "hexagon", "octagon", "decagon"),
#'          filename = NULL, verbose = TRUE,
#'          license = TRUE, bot = FALSE)
#'
#' @param name      a character specifying the name of the city as provided by \code{list_cities()}.
#' @param zoom      a numeric value specifying the amount of zoom. Values > 1 increase zoom and values < 1 decrease zoom. The zoom can be used to speed up rendering of large cities.
#' @param theme     a character specifying the theme of the plot. Possible options are \code{original}, \code{light}, \code{dark}, \code{colored}, \code{rouge}, \code{verde}, \code{neon}, and \code{lichtenstein}.
#' @param border    a character specifying the type of border to use. Possible options are \code{none}, \code{circle}, \code{rhombus}, \code{square}, \code{hexagon} (6 vertices), \code{octagon} (8 vertices), and \code{decagon} (10 vertices).
#' @param filename  character. If specified, the function exports the plot at an appropriate size and does NOT return a \code{ggplot2} object.
#' @param verbose   logical. Whether to show a progress bar during execution.
#' @param license   logical. Whether to add the OpenStreetMap licence to the plot.
#' @param bot       logical. Choose automatically between cities with the same name and add a copyright licence to the image. Primarily used by the twitter bot.
#'
#' @author Koen Derks, \email{koen-derks@hotmail.com}
#'
#' @seealso \code{\link{list_cities}}
#'
#' @keywords create cities
#'
#' @examples
#' \dontrun{
#' # Create a city view of Amsterdam in a circle
#' cityview(name = "Amsterdam", theme = "original", border = "circle")
#' }
#' @export

cityview <- function(name, zoom = 1,
                     theme = c(
                       "original", "light", "dark", "colored",
                       "rouge", "verde", "neon", "lichtenstein"
                     ),
                     border = c(
                       "none", "circle", "rhombus", "square",
                       "hexagon", "octagon", "decagon"
                     ),
                     filename = NULL, verbose = TRUE,
                     license = TRUE, bot = FALSE) {
  theme <- match.arg(theme)
  border <- match.arg(border)
  # Set theme options ##########################################################
  opts <- .theme_options(theme)
  # Look up city ###############################################################
  cities <- rcityviews::cities
  cityIndex <- which(cities$name == name)
  cityIndex <- .resolveIndexConflicts(name, cityIndex, cities, bot)
  row <- cities[cityIndex, ]
  if (bot) {
    cat(paste0(row[["name"]], ", ", row[["country"]]))
  }
  if (verbose) {
    ticks <- 61 + as.numeric(!is.null(filename))
    progBar <- progress::progress_bar$new(format = "  :spin [:bar] :percent | Time remaining: :eta", total = ticks, clear = FALSE, show_after = 0, force = bot)
    progBar$tick(0)
    progBar$message(paste0("Requesting \u00A9 OpenStreetMap features for ", name, ", ", row$country))
  }
  # Create the bounding box ####################################################
  defaultRadius <- 0.0225
  radius <- geosphere::distm(x = c(row[["long"]], row[["lat"]]), y = c(row[["long"]], row[["lat"]] + defaultRadius * (1 / zoom)), fun = geosphere::distHaversine)
  cropped <- data.frame(lat = row[["lat"]], long = row[["long"]]) |>
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326) |>
    sf::st_buffer(dist = radius)
  newbox <- lapply(sf::st_geometry(cropped), sf::st_bbox)[[1]]
  borders <- .make_circle(long = row[["long"]], lat = row[["lat"]], rlong = (abs(newbox$xmax) - abs(newbox$xmin)) / 2 * 1.0075, rlat = (abs(newbox$ymax) - abs(newbox$ymin)) / 2 * 1.0075)
  box <- c(newbox$xmin, newbox$ymin, newbox$xmax, newbox$ymax)
  # Create the border ##########################################################
  if (border == "rhombus") {
    borders <- data.frame(
      x = c(box[1], row[["long"]], box[3], row[["long"]], box[1]),
      y = c(row[["lat"]], box[2], row[["lat"]], box[4], row[["lat"]])
    )
    cropped <- sf::st_sf(sf::st_sfc(sf::st_polygon(list(as.matrix(borders)))), crs = 4326)
  } else if (border == "square") {
    borders <- data.frame(
      x = c(box[1], box[1], box[3], box[3], box[1]),
      y = c(box[2], box[4], box[4], box[2], box[2])
    )
    cropped <- sf::st_sf(sf::st_sfc(sf::st_polygon(list(as.matrix(borders)))), crs = 4326)
  } else if (border == "hexagon" | border == "octagon" | border == "decagon") {
    nsides <- switch(border,
      "hexagon" = 6,
      "octagon" = 8,
      "decagon" = 10
    )
    borders <- data.frame(
      x = row[["long"]] + (abs(box[3]) - abs(box[1])) / 2 * cos(2 * pi * 0:nsides / nsides),
      y = row[["lat"]] + (abs(box[4]) - abs(box[2])) / 2 * sin(2 * pi * 0:nsides / nsides)
    )
    cropped <- sf::st_sf(sf::st_sfc(sf::st_polygon(list(as.matrix(borders)))), crs = 4326)
    newbox <- lapply(sf::st_geometry(cropped), sf::st_bbox)[[1]]
    box <- c(newbox$xmin, newbox$ymin, newbox$xmax, newbox$ymax)
  }
  # Crop the bounding box to the border ########################################
  osmbox <- osmdata::opq(bbox = box, timeout = 900)
  # Initialize an empty plot ###################################################
  int_p <- ggplot2::ggplot()
  .tick(progBar, verbose)
  # Note that the following looks like a lot of unnecessary work but coding it
  # this way minimizes the change of a `HTTP 504 - Gateway Timeout` error.
  # Ocean and land features get special treatment ##############################
  query <- osmdata::osmdata_sf(q = osmdata::add_osm_feature(opq = osmbox, key = "natural", value = "coastline"))
  if (!is.null(query$osm_lines)) {
    motherObj <- .line2poly(obj = query$osm_lines, bbox = box)
    if (!is.null(motherObj[["sea"]])) {
      obj <- .checkAndCrop(motherObj[["sea"]]$geometry, cropped, border)
      int_p <- int_p + ggplot2::geom_sf(data = obj, fill = opts[["water"]], color = opts[["water.line"]], size = 0.3, inherit.aes = FALSE)
    }
    if (!is.null(motherObj[["land"]])) {
      obj <- .checkAndCrop(motherObj[["land"]]$geometry, cropped, border)
      int_p <- int_p + ggplot2::geom_sf(data = obj, fill = opts[["background"]], color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
    }
    if (!is.null(motherObj[["islands"]])) {
      obj <- .checkAndCrop(motherObj[["islands"]]$geometry, cropped, border)
      int_p <- int_p + ggplot2::geom_sf(data = obj, fill = opts[["background"]], color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
    }
  }
  obj <- .get_features(osmbox, cropped, border, features = "\"natural\"=\"coastline\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = opts[["background"]], color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  # Landuse ####################################################################
  obj <- .get_features(osmbox, cropped, border, features = "\"leisure\"=\"park\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = sample(opts[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE), color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"landuse\"=\"forest\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = sample(opts[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE), color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"natural\"=\"wood\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = sample(opts[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE), color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"landuse\"=\"grass\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = sample(opts[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE), color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"natural\"=\"scrub\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = sample(opts[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE), color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"natural\"=\"mud\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = sample(opts[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE), color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"natural\"=\"beach\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = sample(opts[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE), color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"landuse\"=\"meadow\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = sample(opts[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE), color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"landuse\"=\"farmland\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = sample(opts[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE), color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"natural\"=\"wood\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = sample(opts[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE), color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"landuse\"=\"cemetery\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = sample(opts[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE), color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"landuse\"=\"construction\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = sample(opts[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE), color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"amenity\"=\"parking\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = sample(opts[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE), color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"leisure\"=\"playground\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = sample(opts[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE), color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"leisure\"=\"pitch\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = sample(opts[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE), color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  dog_park <- .get_features(osmbox, cropped, border, features = "\"leisure\"=\"dog_park\"")
  int_p <- int_p + ggplot2::geom_sf(data = dog_park[["polygons"]], fill = sample(opts[["landuse"]], size = length(dog_park[["polygons"]]), replace = TRUE), color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"leisure\"=\"garden\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = sample(opts[["landuse"]], size = length(obj[["polygons"]]), replace = TRUE), color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  # Water ######################################################################
  obj <- .get_features(osmbox, cropped, border, features = "\"waterway\"=\"river\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], fill = opts[["water"]], color = opts[["water.line"]], size = 0.6, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"waterway\"=\"canal\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], fill = opts[["water"]], color = opts[["water.line"]], size = 0.5, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"natural\"=\"water\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = opts[["water"]], color = opts[["water.line"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  # Islands ####################################################################
  obj <- .get_features(osmbox, cropped, border, features = "\"natural\"=\"wetland\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = opts[["background"]], color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"place\"=\"islet\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = opts[["background"]], color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"man_made\"=\"pier\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"waterway\"=\"dock\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  # Water lines ################################################################
  obj <- .get_features(osmbox, cropped, border, features = "\"waterway\"=\"riverbank\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["water"]], size = 0.4, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"waterway\"=\"stream\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["water"]], size = 0.4, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"waterway\"=\"ditch\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["water"]], size = 0.4, inherit.aes = FALSE)
  .tick(progBar, verbose)
  # Landuse lines ##############################################################
  obj <- .get_features(osmbox, cropped, border, features = "\"natural\"=\"coastline\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"natural\"=\"peninsula\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"place\"=\"archipelago\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  # Airports ###################################################################
  obj <- .get_features(osmbox, cropped, border, features = "\"aeroway\"=\"taxiway\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.7, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"aeroway\"=\"runway\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"railway\"=\"rail\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["rails"]], size = 0.35, inherit.aes = FALSE)
  .tick(progBar, verbose)
  # Small streets ##############################################################
  obj <- .get_features(osmbox, cropped, border, features = "\"waterway\"=\"dam\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.5, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"waterway\"=\"lock_gate\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.5, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"waterway\"=\"sluice_gate\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.5, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"waterway\"=\"bridge\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.5, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"highway\"=\"footway\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.1, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"highway\"=\"cycleway\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.1, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"highway\"=\"pedestrian\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.1, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"highway\"=\"path\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.1, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"highway\"=\"residential\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.4, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"highway\"=\"living_street\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.4, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"highway\"=\"unclassified\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.4, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"highway\"=\"service\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.4, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"highway\"=\"construction\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.4, inherit.aes = FALSE)
  .tick(progBar, verbose)
  # Medium streets #############################################################
  obj <- .get_features(osmbox, cropped, border, features = "\"highway\"=\"tertiary_link\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.55, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"highway\"=\"tertiary\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = NA, color = opts[["lines"]], size = 0.6, inherit.aes = FALSE)
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.55, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"highway\"=\"secondary_link\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.6, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"highway\"=\"secondary\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = NA, color = opts[["lines"]], size = 0.6, inherit.aes = FALSE)
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.6, inherit.aes = FALSE)
  .tick(progBar, verbose)
  # Large streets ##############################################################
  obj <- .get_features(osmbox, cropped, border, features = "\"highway\"=\"primary_link\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.7, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"highway\"=\"primary\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = NA, color = opts[["lines"]], size = 0.7, inherit.aes = FALSE)
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.7, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"highway\"=\"trunk_link\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.7, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"highway\"=\"trunk_loop\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.7, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"highway\"=\"trunk\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = NA, color = opts[["lines"]], size = 0.7, inherit.aes = FALSE)
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.7, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"highway\"=\"motorway_link\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.8, inherit.aes = FALSE)
  .tick(progBar, verbose)
  obj <- .get_features(osmbox, cropped, border, features = "\"highway\"=\"motorway\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["lines"]], color = opts[["lines"]], size = 0.8, inherit.aes = FALSE)
  .tick(progBar, verbose)
  # Buildings ##################################################################
  obj <- .get_features(osmbox, cropped, border, features = "\"building\"")
  int_p <- int_p + ggplot2::geom_sf(data = obj[["polygons"]], fill = sample(opts[["buildings"]], size = length(obj[["polygons"]]), replace = TRUE), color = opts[["lines"]], size = 0.3, inherit.aes = FALSE)
  .tick(progBar, verbose)
  # Specify coordinate system for plot #########################################
  int_p <- int_p + ggplot2::coord_sf(xlim = c(box[1], box[3]), ylim = c(box[2], box[4]), expand = TRUE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(4, 0, 0, 0, "cm"))
  # Draw the border of the plot ################################################
  if (border != "none") {
    suppressMessages(expr = {
      int_p <- int_p + ggplot2::geom_sf(data = cropped, fill = NA, color = opts[["background"]], size = 1) +
        ggplot2::geom_path(data = borders, mapping = ggplot2::aes(x = x, y = y), color = opts[["text"]], size = 1, inherit.aes = FALSE)
    })
  }
  # Add the city name to the plot ##############################################
  plotName <- if (theme %in% c("light", "dark")) paste0("\u2014", row$name, "\u2014") else row$name
  p <- cowplot::ggdraw(int_p) +
    cowplot::draw_text(text = plotName, x = 0.5, y = 0.93, size = 110, color = opts[["text"]], family = opts[["font"]], fontface = opts[["face"]]) +
    cowplot::draw_text(text = row$country, x = 0.5, y = 0.975, size = 50, color = opts[["text"]], family = opts[["font"]]) +
    ggspatial::annotation_north_arrow(
      location = "bl", height = ggplot2::unit(4, "cm"), width = ggplot2::unit(4, "cm"),
      pad_x = ggplot2::unit(1, "cm"), pad_y = ggplot2::unit(1, "cm"),
      style = ggspatial::north_arrow_nautical(line_col = opts[["text"]], text_size = 25, text_face = opts[["face"]], text_family = opts[["font"]], text_col = opts[["text"]], fill = c(opts[["text"]], opts[["background"]]))
    ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = opts[["background"]], color = opts[["lines"]]),
      panel.background = ggplot2::element_rect(fill = opts[["background"]], color = opts[["background"]])
    )
  if (theme == "lichtenstein") { # Halftone
    p <- .with_halftone(p)
  }
  # Add the coordinates to the plot ############################################
  if (row[["lat"]] < 0) {
    lat <- paste0(format(abs(row[["lat"]]), digits = 6), "\u00B0 S")
  } else {
    lat <- paste0(format(row[["lat"]], digits = 6), "\u00B0 N")
  }
  if (row[["long"]] < 0) {
    long <- paste0(format(abs(row[["long"]]), digits = 6), "\u00B0 W")
  } else {
    long <- paste0(format(row[["long"]], digits = 6), "\u00B0 E")
  }
  p <- p + cowplot::draw_text(text = paste0(lat, " / ", long), x = 0.97, y = 0.03, size = 40, color = opts[["text"]], family = opts[["font"]], hjust = 1)
  # Add the OpenStreetMap licence to the plot ##################################
  if (license) {
    p <- p + cowplot::draw_text(text = "Data by \u00A9 OpenStreetMap contributors", x = 0.97, y = 0.01, size = 20, color = opts[["text"]], family = opts[["font"]], hjust = 1)
  }
  # Save or return the plot ####################################################
  if (is.null(filename)) {
    .tick(progBar, verbose)
    return(p)
  } else {
    .tick(progBar, verbose)
    ggplot2::ggsave(filename = filename, plot = p, height = 500, width = 500, units = "mm", dpi = 100)
    .tick(progBar, verbose)
    return(invisible())
  }
}
