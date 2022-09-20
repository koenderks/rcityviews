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
#'            "original", "light", "dark", "colored", "rouge",
#'            "verde", "neon", "delftware", "vintage", "lichtenstein"
#'          ),
#'          border = c(
#'            "none", "circle", "rhombus", "square",
#'            "hexagon", "octagon", "decagon"
#'          ),
#'          halftone = c("none", "light", "dark"),
#'          places = 0,
#'          legend = FALSE,
#'          filename = NULL,
#'          verbose = TRUE,
#'          license = TRUE,
#'          bot = FALSE)
#'
#' @param name     a character specifying the name of the city as provided by \code{list_cities()}. If \code{NULL} (default), chooses a random city.
#' @param zoom     a numeric value specifying the amount of zoom. Values > 1 increase zoom and values < 1 decrease zoom. The zoom can be used to speed up rendering of large cities.
#' @param theme    a character specifying the theme of the plot, or a list of colors. See the details section for more information. Possible options are \code{original}, \code{light}, \code{dark}, \code{colored}, \code{rouge}, \code{verde}, \code{neon}, \code{delftware}, \code{vintage} and \code{lichtenstein}.
#' @param border   a character specifying the type of border to use. Possible options are \code{none} (default), \code{circle}, \code{rhombus}, \code{square}, \code{hexagon} (6 vertices), \code{octagon} (8 vertices) and \code{decagon} (10 vertices).
#' @param halftone a character specifying the type of halftone to use. Possible options are \code{none}, \code{light} (white dither) and \code{dark} (black dither).
#' @param places   an integer specifying how many suburb, quarter and neighbourhood names to add to the image.
#' @param legend   logical. Whether to add a distance measurer and a compass in the bottom left corner of the image.
#' @param filename character. If specified, the function exports the plot at an appropriate size and does not return a \code{ggplot2} object.
#' @param verbose  logical. Whether to show a progress bar during execution.
#' @param license  logical. Whether to add the OpenStreetMap licence to the plot.
#' @param bot      logical. Enable functionality used by the Twitter bot.
#'
#' @details The \code{theme} argument can take a custom list as input. This list must contain the following elements:
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
#' cityview(name = "Amsterdam", theme = "original", border = "circle")
#' }
#' @export

cityview <- function(name = NULL,
                     zoom = 1,
                     theme = c(
                       "original", "light", "dark", "colored", "rouge",
                       "verde", "neon", "delftware", "vintage", "lichtenstein"
                     ),
                     border = c(
                       "none", "circle", "rhombus", "square",
                       "hexagon", "octagon", "decagon"
                     ),
                     halftone = c("none", "light", "dark"),
                     places = 0,
                     legend = FALSE,
                     filename = NULL,
                     verbose = TRUE,
                     license = TRUE,
                     bot = FALSE) {
  if (is.list(theme)) {
    themeOptions <- theme
  } else {
    theme <- match.arg(theme)
    themeOptions <- .themeOptions(theme)
  }
  border <- match.arg(border)
  halftone <- match.arg(halftone)
  ticks <- 61 + as.numeric(halftone != "none") + as.numeric(places > 0)
  # Set theme options ##########################################################
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
  # Crop the bounding box to the border ########################################
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
        places = places,
        legend = legend,
        cropped = boundaries[["cropped"]],
        borderPoints = boundaries[["borderPoints"]],
        verbose = verbose,
        license = license,
        ticks = ticks,
        shiny = FALSE
      )
    },
    silent = TRUE
  )
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
