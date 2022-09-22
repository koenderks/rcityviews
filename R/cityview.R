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
#' @usage cityview(name = NULL,
#'          zoom = 1,
#'          theme = c(
#'            "vintage", "modern", "bright", "delftware",
#'            "comic", "rouge", "original"
#'          ),
#'          border = c(
#'            "none", "circle", "rhombus", "square",
#'            "hexagon", "octagon", "decagon"
#'          ),
#'          halftone = NULL,
#'          legend = FALSE,
#'          places = 0,
#'          license = TRUE,
#'          filename = NULL,
#'          verbose = TRUE,
#'          bot = FALSE)
#'
#' @param name     a character specifying the name of the city as provided by \code{list_cities()}. If \code{NULL} (default), chooses a random city.
#' @param zoom     a numeric value specifying the amount of zoom. Values > 1 increase zoom and values < 1 decrease zoom. The zoom can be used to speed up rendering of large cities.
#' @param theme    a character specifying the theme of the plot, or a named list specifying a custom theme (see the details section for more information about the composition of this list). Possible pre-specified themes are \code{vintage} (default), \code{modern}, \code{bright}, \code{delftware}, \code{comic}, \code{rouge} and \code{original}.
#' @param border   a character specifying the type of border to use. Possible options are \code{none} (default), \code{circle}, \code{rhombus}, \code{square}, \code{hexagon} (6 vertices), \code{octagon} (8 vertices) and \code{decagon} (10 vertices).
#' @param halftone a character specifying the color of halftone to use. Possible options are \code{none}, \code{light} (white dither) and \code{dark} (black dither).
#' @param legend   logical. Whether to add a distance measurer and a compass in the bottom left corner of the image.
#' @param places   an integer specifying how many suburb, quarter and neighbourhood names to add to the image.
#' @param license  logical. Whether to add the OpenStreetMap licence to the plot.
#' @param filename character. If specified, the function exports the plot at an appropriate size and does not return a \code{ggplot2} object.
#' @param verbose  logical. Whether to show a progress bar during execution.
#' @param bot      logical. Enable functionality used by the Twitter bot.
#'
#' @details The \code{theme} argument can take a custom list as input (see the example). This list must contain the following elements:
#'
#' \code{colors}
#' \itemize{
#'  \item{\code{background}:  One color for the background.}
#'  \item{\code{water}:       One color for the water.}
#'  \item{\code{landuse}:     One color or a vector of multiple colors for the landuse.}
#'  \item{\code{contours}:    One color for the contours of landuse and buildings.}
#'  \item{\code{streets}:     One color for the streets.}
#'  \item{\code{rails}:       One color or a vector of two colors for the rails.}
#'  \item{\code{buildings}:   One color or a vector of multiple colors for the buildings.}
#'  \item{\code{text}:        One color for the text.}
#' }
#' \code{font}
#' \itemize{
#'  \item{\code{family}:      The family of the font.}
#'  \item{\code{face}:        The face of the font.}
#'  \item{\code{append}:      Optional. A string to append the city name with at both sides.}
#' }
#' \code{size}
#' \itemize{
#'  \item{\code{borders}:    A named list containing sizes for the borders \code{contours}, \code{water}, \code{canal} and \code{river}.}
#'  \item{\code{streets}:    A named list contianing sizes for the streets \code{path}, \code{residential}, \code{structure}, \code{tertiary}, \code{secondary}, \code{primary}, \code{motorway}, \code{rails} and \code{runway}.}
#' }
#'
#' @author Koen Derks, \email{koen-derks@hotmail.com}
#'
#' @seealso \code{\link{list_cities}} \code{\link{cityview_shiny}}
#'
#' @keywords create cities
#'
#' @examples
#' \dontrun{
#' # Create a city view of Amsterdam in a circle
#' cityview(name = "Amsterdam", border = "circle", filename = "Amsterdam.png")
#'
#' # Use a custom theme, for example green and white, streets only
#' myTheme <- list(
#'   colors = list(
#'     background = "forestgreen",
#'     water = NA,
#'     landuse = NA,
#'     contours = NA,
#'     streets = "white",
#'     rails = c("white", "forestgreen"),
#'     buildings = NA,
#'     text = "white",
#'     waterlines = NA
#'   ),
#'   font = list(
#'     family = "serif",
#'     face = "bold"
#'   ),
#'   size = list(
#'     borders = list(
#'       contours = 0.3,
#'       water = 0.4,
#'       canal = 0.5,
#'       river = 0.6
#'     ),
#'     streets = list(
#'       path = 0.1,
#'       residential = 0.4,
#'       structure = 0.5,
#'       tertiary = 0.75,
#'       secondary = 0.8,
#'       primary = 0.9,
#'       motorway = 1,
#'       rails = 0.75,
#'       runway = 3
#'     )
#'   )
#' )
#' cityview(name = "Amsterdam", theme = myTheme, filename = "Amsterdam.png")
#' }
#' @export

cityview <- function(name = NULL,
                     zoom = 1,
                     theme = c(
                       "vintage", "modern", "bright", "delftware",
                       "comic", "rouge", "original"
                     ),
                     border = c(
                       "none", "circle", "rhombus", "square",
                       "hexagon", "octagon", "decagon"
                     ),
                     halftone = NULL,
                     legend = FALSE,
                     places = 0,
                     license = TRUE,
                     filename = NULL,
                     verbose = TRUE,
                     bot = FALSE) {
  # Set image options ##########################################################
  if (is.list(theme)) {
    themeOptions <- theme
  } else {
    theme <- match.arg(theme)
    themeOptions <- .themeOptions(theme)
  }
  border <- match.arg(border)
  ticks <- 61 + as.numeric(!is.null(halftone)) + as.numeric(places > 0)
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
  bbox <- osmdata::opq(bbox = boundaries[["panel"]], timeout = 25)
  # Build the plot #############################################################
  try <- try(
    {
      image <- .buildCity(
        city = city,
        bbox = bbox,
        zoom = zoom,
        panel = boundaries[["panel"]],
        themeOptions = themeOptions,
        border = border,
        halftone = halftone,
        legend = legend,
        places = places,
        cropped = boundaries[["cropped"]],
        borderPoints = boundaries[["borderPoints"]],
        license = license,
        verbose = verbose,
        ticks = ticks,
        shiny = FALSE
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
