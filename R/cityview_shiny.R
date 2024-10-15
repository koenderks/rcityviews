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

#' [Shiny] Create a City View
#'
#' @description Launch a Shiny app in which you can create a city view
#'   showcasing a particular city or region using OpenStreetMap (OSM) data
#'   retreived trough the Overpass API. Please note that OpenStreetMap is open
#'   data and can be freely utilized for any purpose, as long as proper credit
#'   is given to OpenStreetMap and its contributors.
#'
#' @usage cityview_shiny(launch.browser = FALSE)
#'
#' @param launch.browser logical. If \code{FALSE} (the default), the app will
#'                       open in the viewer panel. If \code{TRUE}, the app will
#'                       open in the system's default web browser.
#'
#' @author Koen Derks, \email{koen-derks@hotmail.com}
#'
#' @seealso \code{\link{list_cities}}
#'          \code{\link{cityview}}
#'          \code{\link{new_city}}
#'          \code{\link{city_themes}}
#'
#' @keywords create cities shiny
#'
#' @references \url{https://www.openstreetmap.org}
#'
#' @examples
#' \dontrun{
#' # Start the shiny app
#' cityview_shiny()
#' }
#' @export
#' @importFrom svglite svglite
cityview_shiny <- function(launch.browser = FALSE) {
  shiny::shinyApp(
    ui = .shiny_ui, server = .shiny_server,
    options = if (launch.browser) list("launch.browser" = TRUE) else NULL
  )
}
