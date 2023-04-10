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

#' Specify a New City
#'
#' @description Define a city using country, latitude and longitude coordinates.
#'   The returned object can be used a input for \code{name} argument the
#'   \code{cityview()} function.
#'
#'
#' @usage new_city(name = NULL, country = NULL, lat = NULL, long = NULL)
#'
#' @param name      string to be printed as the city name.
#' @param country   string to be printed as the country.
#' @param lat       numeric value to be used as the latitude.
#' @param long      numeric value to be used as the longitude.
#'
#' @return a data frame containing the new city alongside its respective
#'   country and coordinates.
#'
#' @author Koen Derks, \email{koen-derks@hotmail.com}
#'
#' @seealso \code{\link{cityview}} \code{\link{cityview_shiny}}
#'   \code{\link{list_cities}}
#'
#' @keywords cities new
#'
#' @examples
#' city <- new_city(
#'   name = "Lagos", country = "Portugal",
#'   lat = 37.10, long = -8.68
#' )
#' \dontrun{
#' cityview(name = city)
#' }
#' @export

new_city <- function(name = NULL, country = NULL, lat = NULL, long = NULL) {
  stopifnot("specify all input arguments" = all(c(!is.null(name), !is.null(country), !is.null(lat), !is.null(long))))
  stopifnot("all input must be of length 1" = all(c(length(name) == 1, length(country) == 1, length(lat) == 1, length(long) == 1)))
  stopifnot("'lat' must be >= -90 and <= 90" = lat >= -90 && lat <= 90)
  stopifnot("'long' must be >= -180 and <= 180" = long >= -180 && long <= 180)
  out <- data.frame("name" = name, "country" = country, "lat" = lat, "long" = long)
  message(paste0("Discovered the city of ", name, ", ", country, " at ", lat, "\u00B0 / ", long, "\u00B0!"))
  return(out)
}
