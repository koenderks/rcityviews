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

#' Specify a New City
#'
#' @description Define a city using country, latitude and longitude coordinates.
#'   The returned object can be used a input for \code{name} argument the
#'   \code{cityview()} function.
#'
#'
#' @usage new_city(name = NULL, country = NULL, lat = NULL, long = NULL)
#'
#' @param name    A single string to be used as the city name.
#' @param country A single string to be used as the country.
#' @param lat     A single numeric value to be used as the latitude.
#' @param long    A single numeric value to be used as the longitude.
#' @param method  A character string specifying the geocoding method to use when the user does not specify \code{lat} or \code{long}. Defaults to "osm" (OpenStreetMap). Supported methods include:
#'   \code{"osm"}, \code{"census"}, \code{"arcgis"}, \code{"census_simple"}, \code{"geocodio"}, \code{"mapbox"},
#'   \code{"google"}, \code{"bing"}, \code{"here"}, \code{"tomtom"}, \code{"nominatim"}, and \code{"tiger"}. Visit \url{https://jessecambon.github.io/tidygeocoder/reference/geo.html} for more details
#'
#' @return a data frame containing the new city alongside its respective
#'   country and coordinates.
#'
#' @author Koen Derks, \email{koen-derks@hotmail.com}
#'
#' @seealso \code{\link{cityview}}
#'          \code{\link{cityview_shiny}}
#'          \code{\link{list_cities}}
#'          \code{\link{city_themes}}
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

new_city <- function(name = NULL, country = NULL, lat = NULL, long = NULL, method = c(
                       "osm", "census",
                       "arcgis", "census_simple", "geocodio", "mapbox", "google", "bing", "here", "tomtom", "nominatim", "tiger"
                     )) {
  method <- match.arg(method)
  stopifnot("At least provide a location name and country" = all(c(length(name) == 1, length(country) == 1)))
  if (is.null(lat) || is.null(long)) {
    out <- .geocode(name, country, method)
    lat <- out$lat
    long <- out$long
  }
  stopifnot("'lat' must be >= -90 and <= 90" = lat >= -90 && lat <= 90)
  stopifnot("'long' must be >= -180 and <= 180" = long >= -180 && long <= 180)
  message(paste0("Discovered the city of ", name, ", ", country, " at ", lat, "\u00B0 / ", long, "\u00B0!"))
  out <- data.frame("name" = name, "country" = country, "lat" = lat, "long" = long)
  class(out) <- c("rcityviewsCity", "data.frame")
  return(out)
}
