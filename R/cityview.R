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

#' Create a City View
#'
#' @description Create a city view showcasing a particular city or region using
#'   OpenStreetMap (OSM) data retreived trough the Overpass API. Please note
#'   that OpenStreetMap is open data and can be freely utilized for any purpose,
#'   as long as proper credit is given to OpenStreetMap and its contributors.
#'
#'
#' @usage cityview(
#'   name = NULL,
#'   zoom = 1,
#'   theme = c(
#'     "vintage", "modern", "bright", "delftware", "comic",
#'     "rouge", "original", "midearth", "batik", "vice"
#'   ),
#'   border = c(
#'     "none", "circle", "rhombus", "square",
#'     "hexagon", "octagon", "decagon", "bbox"
#'   ),
#'   halftone = NULL,
#'   legend = FALSE,
#'   places = 0,
#'   license = TRUE,
#'   timeout = 25,
#'   filename = NULL,
#'   verbose = TRUE,
#'   cache = TRUE,
#'   bot = FALSE
#' )
#'
#' @param name     a character specifying the name of the city as provided by
#'                 \code{list_cities()}, or an object created using
#'                 \code{new_city()}, or a row of the ouput of
#'                 \code{list_cities()}. If \code{NULL} (default), chooses a
#'                 random city.
#' @param zoom     a numeric value specifying the amount of zoom. Values > 1
#'                 increase zoom and speed up computation time, while values < 1
#'                 decrease zoom and increase computation time. For zoom levels
#'                 below 0.5, the computation time can be very long.
#' @param theme    a character specifying the theme of the plot, or a named list
#'                 specifying a custom theme (see the details section for more
#'                 information about the composition of this list). Possible
#'                 pre-specified themes are \code{vintage} (default),
#'                 \code{modern}, \code{bright}, \code{delftware}, \code{comic},
#'                 \code{rouge}, \code{original}, \code{midearth}, \code{batik}
#'                 and \code{vice}.
#' @param border   a character specifying the type of border to use. Possible
#'                 options are \code{none} (default), \code{circle},
#'                 \code{rhombus}, \code{square}, \code{hexagon} (6 vertices),
#'                 \code{octagon} (8 vertices), \code{decagon} (10 vertices) and
#'                 \code{bbox} (the bounding box for the entire city, argument
#'                 \code{zoom} will be ignored).
#' @param halftone a character specifying the color of applied halftone dither.
#' @param legend   logical. Whether to add a distance measurer and a compass in
#'                 the bottom left corner of the image.
#' @param places   an integer specifying how many suburb, quarter and
#'                 neighbourhood names to add to the image.
#' @param license  logical. Whether to add the OpenStreetMap licence in the
#'                 bottom right corner of the figure.
#' @param timeout  a value specifying the timeout (in seconds) for the Overpass
#'                 server. It may be necessary to increase this value for large
#'                 or populated areas because the server may time out before all
#'                 data are delivered (if this occurs you will receive the
#'                 following error message: \code{runtime error: Query timed out
#'                 in "recurse" at line ... after ... seconds}.
#' @param filename character. If specified, the function exports the plot at an
#'                 appropriate size and does not return a \code{ggplot2} object.
#' @param verbose  logical. Whether to show a progress bar during execution.
#' @param cache    logical. Whether to cache the data for the image so that they
#'                 do not need to be requested again when calling the function
#'                 with a different theme.
#' @param bot      logical. Enable functionality used by the Twitter bot.
#'
#' @details The \code{theme} argument can take a custom list as input (see the
#'   example). This list must contain all of the following elements:
#'
#' \code{colors}
#' \itemize{
#'  \item{\code{background}:  A single color to be used for the background.}
#'  \item{\code{water}:       A single color to be used for the water.}
#'  \item{\code{landuse}:     A single color or a vector of multiple colors to
#'                            be used for the landuse.}
#'  \item{\code{contours}:    A single color to be used for the contours (lines)
#'                            of landuse and buildings.}
#'  \item{\code{streets}:     A single color to be used for the streets.}
#'  \item{\code{rails}:       A single color or a vector of two colors to be
#'                            used for the rails.}
#'  \item{\code{buildings}:   A single color or a vector of multiple colors to
#'                            be used for the buildings.}
#'  \item{\code{text}:        A single color to be used for the text.}
#' }
#' \code{font}
#' \itemize{
#'  \item{\code{family}:      A string specifying the family of the font.}
#'  \item{\code{face}:        A string specifying the face of the font.}
#'  \item{\code{scale}:       A single value specifying the expansion factor of
#'                            the characters in the font.}
#'  \item{\code{append}:      Optional. A string to append the city name with at
#'                            both sides.}
#' }
#' \code{size}
#' \itemize{
#'  \item{\code{borders}:    A named list containing sizes for the borders
#'                           \code{contours}, \code{water}, \code{canal} and
#'                           \code{river}.}
#'  \item{\code{streets}:    A named list contianing sizes for the streets
#'                           \code{path}, \code{residential}, \code{structure},
#'                           \code{tertiary}, \code{secondary}, \code{primary},
#'                           \code{motorway}, \code{rails} and \code{runway}.}
#' }
#'
#' @author Koen Derks, \email{koen-derks@hotmail.com}
#'
#' @seealso \code{\link{list_cities}}
#'          \code{\link{cityview_shiny}}
#'          \code{\link{new_city}}
#'          \code{\link{city_themes}}
#'
#' @keywords create cities
#'
#' @references \url{https://www.openstreetmap.org}
#'
#' @examples
#' \dontrun{
#' # 1. Simple example
#' # Create a city view of Amsterdam in a circle
#' cityview(name = "Amsterdam", border = "circle")
#'
#' # 2. Advanced example
#' # Custom theme (black, beige and white), streets only, direct export
#' myTheme <- list(
#'   colors = list(
#'     background = "#232323",
#'     water = "#232323",
#'     landuse = "#232323",
#'     contours = "#232323",
#'     streets = "#d7b174",
#'     rails = c("#d7b174", "#232323"),
#'     buildings = "#232323",
#'     text = "#ffffff",
#'     waterlines = "#232323"
#'   ),
#'   font = list(
#'     family = "serif",
#'     face = "bold",
#'     scale = 1,
#'     append = "\u2014"
#'   ),
#'   size = list(
#'     borders = list(
#'       contours = 0.15,
#'       water = 0.4,
#'       canal = 0.5,
#'       river = 0.6
#'     ),
#'     streets = list(
#'       path = 0.2,
#'       residential = 0.3,
#'       structure = 0.35,
#'       tertiary = 0.4,
#'       secondary = 0.5,
#'       primary = 0.6,
#'       motorway = 0.8,
#'       rails = 0.75,
#'       runway = 3
#'     )
#'   )
#' )
#' cityview(
#'   name = "Amsterdam", theme = myTheme,
#'   border = "square", filename = "Amsterdam.png"
#' )
#' }
#' @export

cityview <- function(name = NULL,
                     zoom = 1,
                     theme = c(
                       "vintage", "modern", "bright", "delftware", "comic",
                       "rouge", "original", "midearth", "batik", "vice"
                     ),
                     border = c(
                       "none", "circle", "rhombus", "square",
                       "hexagon", "octagon", "decagon", "bbox"
                     ),
                     halftone = NULL,
                     legend = FALSE,
                     places = 0,
                     license = TRUE,
                     timeout = 25,
                     filename = NULL,
                     verbose = TRUE,
                     cache = TRUE,
                     bot = FALSE) {
  # Error handling #############################################################
  stopifnot("argument 'zoom' must be a single number > 0" = !is.null(zoom) && is.numeric(zoom) && length(zoom) == 1L && zoom > 0)
  stopifnot("argument 'legend' must be a single logical" = !is.null(legend) && is.logical(legend) && length(legend) == 1L)
  stopifnot("argument 'places' must be a single integer >= 0" = !is.null(places) && places %% 1 == 0 && places >= 0 && length(places) == 1L)
  stopifnot("argument 'license' must be a single logical" = !is.null(license) && is.logical(license) && length(license) == 1L)
  stopifnot("argument 'timeout' must be a single number >= 0" = !is.null(timeout) && is.numeric(timeout) && timeout >= 0 && length(timeout) == 1L)
  stopifnot("argument 'verbose' must be a single logical" = !is.null(verbose) && is.logical(verbose) && length(verbose) == 1L)
  stopifnot("argument 'cache' must be a single logical" = !is.null(cache) && is.logical(cache) && length(cache) == 1L)
  stopifnot("argument 'bot' must be a single logical" = !is.null(bot) && is.logical(bot) && length(bot) == 1L)
  if (!is.null(halftone)) {
    stopifnot("'halftone' must be a single character representing a valid color" = .isColor(halftone) && length(halftone) == 1L)
  }
  # Set image options ##########################################################
  if (inherits(theme, "rcityview.theme")) {
    themeOptions <- theme
  } else if (is.list(theme)) {
    themeOptions <- theme
    .checkThemeOptions(themeOptions)
  } else {
    theme <- match.arg(theme)
    themeOptions <- .themeOptions(theme)
  }
  border <- match.arg(border)
  # Look up city ###############################################################
  city <- .getCity(name)
  if (is.null(city)) {
    return(invisible())
  }
  if (bot) {
    cat(paste0(city[["name"]], ", ", city[["country"]]))
  }
  # Create the bounding box ####################################################
  boundaries <- .getBoundaries(city = city, border = border, zoom = zoom)
  # Initialize the OSM query ###################################################
  bbox <- osmdata::opq(bbox = boundaries[["panel"]], timeout = timeout)
  # Determine whether to use caching
  .requestData <- if (cache) .memoiseRequestData else .nonMemoiseRequestData
  # Build the plot #############################################################
  try <- try(
    {
      imgData <- .requestData(
        city = city,
        bbox = bbox,
        zoom = boundaries[["zoom"]],
        panel = boundaries[["panel"]],
        border = border,
        cropped = boundaries[["cropped"]],
        verbose = verbose,
        shiny = FALSE
      )
      image <- .buildCity(
        imgData = imgData,
        city = city,
        bbox = bbox,
        zoom = boundaries[["zoom"]],
        panel = boundaries[["panel"]],
        themeOptions = themeOptions,
        border = border,
        halftone = halftone,
        legend = legend,
        places = places,
        cropped = boundaries[["cropped"]],
        borderPoints = boundaries[["borderPoints"]],
        license = license
      )
    },
    silent = TRUE
  )
  # Error handling #############################################################
  if ("try-error" %in% class(try)) {
    if (try[[1]] == "Error in resp_abort(resp, error_body(req, resp)) : \n  HTTP 504 Gateway Timeout.\n") {
      stop("The overpass server is not able to respond to your request, traffic might be too high.")
    } else {
      stop(try[[1]]) # Print original error message
    }
  }
  if (cache && utils::object.size(imgData) > (1024 * 1024^2)) {
    message("The map data is not cached because it exceeds the maximum cache size of 1024 MB.")
  }
  # Save or return the plot ####################################################
  if (is.null(filename)) {
    return(image)
  } else {
    ggplot2::ggsave(
      filename = filename,
      plot = image,
      height = 500,
      width = 500,
      units = "mm",
      dpi = 100
    )
    return(invisible())
  }
}
