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
#'          theme = c("original", "light", "dark", "destination", "rouge", "colored", "neon"),
#'          border = c("none", "circle", "rhombus", "square", "hexagon", "octagon", "decagon"),
#'          filename = NULL, verbose = TRUE, bot = FALSE)
#'
#' @param name      a character specifying the name of the city as provided by \code{list_cities()}.
#' @param zoom      a numeric value specifying the amount of zoom. Values > 1 increase zoom and values < 1 decrease zoom. The zoom can be used to speed up rendering of large cities.
#' @param theme     a character specifying the theme of the plot. Possible options are \code{original}, \code{light}, \code{dark}, \code{destination}, \code{rouge}, \code{colored}, and \code{neon}.
#' @param border    a character specifying the type of border to use. Possible options are \code{none}, \code{circle}, \code{rhombus}, \code{square}, \code{hexagon} (6 vertices), \code{octagon} (8 vertices), and \code{decagon} (10 vertices).
#' @param filename  character. If specified, the function exports the plot at an appropriate size and does NOT return a \code{ggplot2} object.
#' @param verbose   logical. Whether to show a progress bar during execution.
#' @param bot       logical. Choose automatically between cities with the same name and add a copyright licence to the image. Primarily used by the twitter bot.
#' @param colors    if specified, overrides the \code{theme} argument allowing for custom colors. The input for this argument must be a list containing vector of color(s). The list elements correspond to 1) line color (length 1), 2) background color (length 1), 3) water color (length 1), 4) landuse color (length >= 1), 5) text color (length 1) and 6) rail color (length 1).
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
#' cityview(name = "Amsterdam", border = "circle")
#' }
#' @export

cityview <- function(name, zoom = 1,
                     theme = c("original", "light", "dark", "destination", "rouge", "colored", "neon"),
                     border = c("none", "circle", "rhombus", "square", "hexagon", "octagon", "decagon"),
                     filename = NULL, verbose = TRUE, bot = FALSE, colors = NULL) {
  theme <- match.arg(theme)
  border <- match.arg(border)
  if (is.null(colors)) {
    colors <- .theme_colors(theme)
  }
  font <- switch(theme,
    "original" = "Caveat",
    "light" = "Imbue",
    "dark" = "Imbue",
    "rouge" = "Oswald",
    "colored" = "Damion",
    "neon" = "Neonderthaw",
    "destination" = "Wallpoet"
  )
  boldFont <- if (theme %in% c("original", "destination", "rouge", "neon")) "bold" else "plain"
  cities <- rcityviews::cities
  cityIndex <- which(cities$name == name)
  cityIndex <- .resolveIndexConflicts(name, cityIndex, cities, bot)
  row <- cities[cityIndex, ]
  if (bot) {
    cat(paste0(row[["name"]], ", ", row[["country"]]))
  }
  if (verbose) {
    ticks <- 12 + as.numeric(!is.null(filename))
    progBar <- progress::progress_bar$new(format = "  :spin [:bar] :percent | Time remaining: :eta", total = ticks, clear = FALSE, show_after = 0, force = bot)
    progBar$tick(0)
    progBar$message(paste0("Requesting \u00A9 OpenStreetMap features for ", name, ", ", row$country))
  }
  defaultRadius <- 0.0225
  radius <- geosphere::distm(x = c(row[["long"]], row[["lat"]]), y = c(row[["long"]], row[["lat"]] + defaultRadius * (1 / zoom)), fun = geosphere::distHaversine)
  cropped <- data.frame(lat = row[["lat"]], long = row[["long"]]) |>
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326) |>
    sf::st_buffer(dist = radius)
  newbox <- lapply(sf::st_geometry(cropped), sf::st_bbox)[[1]]
  borders <- .make_circle(long = row[["long"]], lat = row[["lat"]], rlong = (abs(newbox$xmax) - abs(newbox$xmin)) / 2 * 1.0075, rlat = (abs(newbox$ymax) - abs(newbox$ymin)) / 2 * 1.0075)
  box <- c(newbox$xmin, newbox$ymin, newbox$xmax, newbox$ymax)
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
  osmbox <- osmdata::opq(bbox = box, timeout = 1000)
  if (verbose) {
    progBar$tick()
  }
  featuresLanduse <- osmdata::add_osm_features(opq = osmbox, features = c(
    "\"landuse\"=\"grass\"",
    "\"landuse\"=\"meadow\"",
    "\"landuse\"=\"farmland\"",
    "\"landuse\"=\"forest\"",
    "\"landuse\"=\"forest\"",
    "\"landuse\"=\"construction\"",
    "\"natural\"=\"scrub\"",
    "\"natural\"=\"straight\"",
    "\"natural\"=\"coastline\"",
    "\"natural\"=\"beach\"",
    "\"natural\"=\"peninsula\"",
    "\"man_made\"=\"pier\"",
    "\"waterway\"=\"dock\"",
    "\"waterway\"=\"dam\"",
    "\"waterway\"=\"lock_gate\"",
    "\"waterway\"=\"sluice_gate\"",
    "\"waterway\"=\"bridge\"",
    "\"amenity\"=\"parking\"",
    "\"leisure\"=\"playground\"",
    "\"leisure\"=\"park\"",
    "\"leisure\"=\"pitch\"",
    "\"leisure\"=\"nature_reserve\""
  ))
  queryLanduse <- osmdata::osmdata_sf(q = featuresLanduse)
  landuseMultipolygons <- .checkAndCrop(queryLanduse$osm_multipolygons$geometry, cropped, border)
  landusePolygons <- .checkAndCrop(queryLanduse$osm_polygons$geometry, cropped, border)
  landuseLines <- .checkAndCrop(queryLanduse$osm_lines$geometry, cropped, border)
  if (verbose) {
    progBar$tick()
  }
  featuresWater <- osmdata::add_osm_features(opq = osmbox, features = c(
    "\"natural\"=\"water\"",
    "\"waterway\"=\"riverbank\"",
    "\"waterway\"=\"stream\"",
    "\"waterway\"=\"ditch\""
  ))
  queryWater <- osmdata::osmdata_sf(q = featuresWater)
  waterMultipolygons <- .checkAndCrop(queryWater$osm_multipolygons$geometry, cropped, border)
  waterPolygons <- .checkAndCrop(queryWater$osm_polygons$geometry, cropped, border)
  if (verbose) {
    progBar$tick()
  }
  featuresBuildings <- osmdata::add_osm_feature(opq = osmbox, key = "building")
  queryBuildings <- osmdata::osmdata_sf(q = featuresBuildings)
  buildingsPolygons <- .checkAndCrop(queryBuildings$osm_polygons$geometry, cropped, border)
  if (verbose) {
    progBar$tick()
  }
  featuresSstreets <- osmdata::add_osm_feature(opq = osmbox, key = "highway", value = c("residential", "living_street", "unclassified", "service"))
  querySstreets <- osmdata::osmdata_sf(q = featuresSstreets)
  sstreetLines <- .checkAndCrop(querySstreets$osm_lines$geometry, cropped, border)
  if (verbose) {
    progBar$tick()
  }
  featuresFstreets <- osmdata::add_osm_feature(opq = osmbox, key = "highway", value = c("footway", "cycleway"))
  queryFstreets <- osmdata::osmdata_sf(q = featuresFstreets)
  fstreetsLines <- .checkAndCrop(queryFstreets$osm_lines$geometry, cropped, border)
  if (verbose) {
    progBar$tick()
  }
  featuresRails <- osmdata::add_osm_feature(opq = osmbox, key = "railway", value = "rail")
  queryRails <- osmdata::osmdata_sf(q = featuresRails)
  railsLines <- .checkAndCrop(queryRails$osm_lines$geometry, cropped, border)
  if (verbose) {
    progBar$tick()
  }
  featuresMstreets <- osmdata::add_osm_feature(opq = osmbox, key = "highway", value = c("secondary", "tertiary", "secondary_link", "tertiary_link"))
  queryMstreets <- osmdata::osmdata_sf(q = featuresMstreets)
  mstreetsLines <- .checkAndCrop(queryMstreets$osm_lines$geometry, cropped, border)
  if (verbose) {
    progBar$tick()
  }
  featuresLstreets <- osmdata::add_osm_feature(opq = osmbox, key = "highway", value = c("motorway", "motorway_link", "primary", "primary_link", "trunk", "trunk_link", "trunk_loop"))
  queryLstreets <- osmdata::osmdata_sf(q = featuresLstreets)
  lstreetsLines <- .checkAndCrop(queryLstreets$osm_lines$geometry, cropped, border)
  if (verbose) {
    progBar$tick()
  }
  featuresTaxiway <- osmdata::add_osm_feature(opq = osmbox, key = "aeroway", value = "taxiway")
  queryTaxiway <- osmdata::osmdata_sf(q = featuresTaxiway)
  taxiwayLines <- .checkAndCrop(queryTaxiway$osm_lines$geometry, cropped, border)
  if (verbose) {
    progBar$tick()
  }
  featuresRunway <- osmdata::add_osm_feature(opq = osmbox, key = "aeroway", value = "runway")
  queryRunway <- osmdata::osmdata_sf(q = featuresRunway)
  runwayLines <- .checkAndCrop(queryRunway$osm_lines$geometry, cropped, border)
  if (verbose) {
    progBar$tick()
  }
  int_p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = waterMultipolygons, fill = colors[[3]], color = colors[[1]], size = 0.3, inherit.aes = FALSE) +
    ggplot2::geom_sf(data = waterPolygons, fill = colors[[3]], color = colors[[1]], size = 0.3, inherit.aes = FALSE) +
    ggplot2::geom_sf(data = landuseMultipolygons, fill = sample(colors[[4]], size = length(landuseMultipolygons), replace = TRUE), color = colors[[1]], size = 0.3, inherit.aes = FALSE) +
    ggplot2::geom_sf(data = landusePolygons, fill = sample(colors[[4]], size = length(landusePolygons), replace = TRUE), color = colors[[1]], size = 0.3, inherit.aes = FALSE) +
    ggplot2::geom_sf(data = landuseLines, color = colors[[1]], size = 0.3, inherit.aes = FALSE) +
    ggplot2::geom_sf(data = taxiwayLines, color = colors[[1]], size = 0.7, inherit.aes = FALSE) +
    ggplot2::geom_sf(data = runwayLines, color = colors[[1]], size = 3, inherit.aes = FALSE) +
    ggplot2::geom_sf(data = railsLines, color = colors[[6]], size = 0.35, inherit.aes = FALSE) +
    ggplot2::geom_sf(data = mstreetsLines, color = colors[[1]], size = 0.6, inherit.aes = FALSE) +
    ggplot2::geom_sf(data = sstreetLines, color = colors[[1]], size = 0.4, inherit.aes = FALSE) +
    ggplot2::geom_sf(data = fstreetsLines, color = colors[[1]], size = 0.1, inherit.aes = FALSE) +
    ggplot2::geom_sf(data = lstreetsLines, color = colors[[1]], size = 0.7, inherit.aes = FALSE) +
    ggplot2::geom_sf(data = buildingsPolygons, fill = sample(colors[[4]], size = length(buildingsPolygons), replace = TRUE), color = colors[[1]], size = 0.25, inherit.aes = FALSE) +
    ggplot2::coord_sf(xlim = c(box[1], box[3]), ylim = c(box[2], box[4]), expand = TRUE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(4, 0, 0, 0, "cm"))
  if (border != "none") {
    suppressMessages(expr = {
      int_p <- int_p + ggplot2::geom_sf(data = cropped, fill = NA, color = colors[[2]], size = 1) +
        ggplot2::geom_path(data = borders, mapping = ggplot2::aes(x = x, y = y), color = colors[[5]], size = 1, inherit.aes = FALSE)
    })
  }
  plotName <- if (theme %in% c("light", "dark")) paste0("\u2014", row$name, "\u2014") else row$name
  p <- cowplot::ggdraw(int_p) +
    cowplot::draw_text(text = plotName, x = 0.5, y = 0.93, size = 110, color = colors[[5]], family = font, fontface = boldFont) +
    cowplot::draw_text(text = row$country, x = 0.5, y = 0.975, size = 50, color = colors[[5]], family = font) +
    ggspatial::annotation_north_arrow(
      location = "bl", height = ggplot2::unit(4, "cm"), width = ggplot2::unit(4, "cm"),
      pad_x = ggplot2::unit(1, "cm"), pad_y = ggplot2::unit(1, "cm"),
      style = ggspatial::north_arrow_nautical(line_col = colors[[5]], text_size = 25, text_face = boldFont, text_family = font, text_col = colors[[5]], fill = c(colors[[5]], colors[[2]]))
    ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = colors[[2]], color = colors[[1]]),
      panel.background = ggplot2::element_rect(fill = colors[[2]], color = colors[[2]])
    )
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
  p <- p + cowplot::draw_text(text = paste0(lat, " / ", long), x = 0.97, y = 0.03, size = 40, color = colors[[5]], family = font, hjust = 1)
  if (bot) {
    p <- p + cowplot::draw_text(text = "Data by \u00A9 OpenStreetMap contributors", x = 0.97, y = 0.01, size = 20, color = colors[[5]], family = font, hjust = 1)
  }
  if (is.null(filename)) {
    if (verbose) {
      progBar$tick()
    }
    return(p)
  } else {
    if (verbose) {
      progBar$tick()
    }
    ggplot2::ggsave(filename = filename, plot = p, height = 500, width = 500, units = "mm", dpi = 100)
    if (verbose) {
      progBar$tick()
    }
    return(invisible())
  }
}
