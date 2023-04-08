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

#' [Shiny] Create an Aerial City View
#'
#' @description Create an aerial city view in Shiny.
#'
#' @usage cityview_shiny(launch.browser = FALSE)
#'
#' @param launch.browser logical. If \code{FALSE} (the default), the app will run in the viewer panel. If \code{TRUE}, the app will run in the system's default web browser.
#'
#' @author Koen Derks, \email{koen-derks@hotmail.com}
#'
#' @seealso \code{\link{list_cities}} \code{\link{cityview}} \code{\link{new_city}}
#'
#' @keywords create cities shiny
#'
#' @examples
#' \dontrun{
#' # Start the shiny app
#' cityview_shiny()
#' }
#' @export
cityview_shiny <- function(launch.browser = FALSE) {
  shiny::shinyApp(
    ui = .shiny_ui, server = .shiny_server,
    options = if (launch.browser) list("launch.browser" = TRUE) else NULL
  )
}
