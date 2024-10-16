# Copyright (C) 2022-2024 Thomas Goossens

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

#' Geocode a Location Using Various Methods
#'
#' @description This function takes a location name as a string and returns the latitude and longitude coordinates using various geocoding methods.
#'   It supports multiple geocoding services and handles API keys where necessary.
#'
#' @usage geocode_raw(location, method = "osm")
#'
#' @param location A character string specifying the location name to geocode. It must be provided as a single string.
#' @param method A character string specifying the geocoding method to use. Defaults to "osm" (OpenStreetMap). Supported methods include:
#'   \code{"osm"}, \code{"census"}, \code{"arcgis"}, \code{"census_simple"}, \code{"geocodio"}, \code{"mapbox"},
#'   \code{"google"}, \code{"bing"}, \code{"here"}, \code{"tomtom"}, \code{"nominatim"}, and \code{"tiger"}.
#'
#' @details This function attempts to geocode the specified location name using the chosen method. If the method requires an API key,
#'   it will check for the necessary environment variable and return an error if the key is missing. The function outputs a data frame
#'   containing the geocoded coordinates and location information.
#'
#' @return A data frame containing the following columns:
#'   \item{name}{The name of the city or location.}
#'   \item{country}{The country in which the location is situated, inferred from the display name.}
#'   \item{lat}{The latitude of the location.}
#'   \item{long}{The longitude of the location.}
#'   \item{population}{The population of the location (not available in this function, returns \code{NA}).}
#'
#' @examples
#' \dontrun{
#'   # Geocode a location using OpenStreetMap (default)
#'   geocode_raw("New York, USA")
#'
#'   # Geocode a location using Google (API key required)
#'   geocode_raw("Tokyo, Japan", method = "google")
#' }
#'
#' @export
geocode_raw <- function(location, method = "osm") {
  if (missing(location) || !is.character(location)) {
    stop("Please provide a valid location name as a string.")
  }
  
  # Use match.arg to validate the method parameter
  method <- match.arg(method, choices = c("osm", "census", "arcgis", "census_simple", "geocodio",
                                          "mapbox", "google", "bing", "here", "tomtom", "nominatim", "tiger"))
  
  # Handle API keys for methods that require them
  methods_with_keys <- c("google", "bing", "here", "tomtom", "mapbox", "geocodio")
  if (method %in% methods_with_keys) {
    api_key_env <- paste0(toupper(method), "_API_KEY")
    api_key <- Sys.getenv(api_key_env)
    if (api_key == "") {
      stop(paste0("API key for ", method, " is required. Please set the '", api_key_env, "' environment variable."))
    }
  }
  
  result <- tryCatch({
    geocode_df <- tibble::tibble(address = location) %>%
      tidygeocoder::geocode(address, method = method, quiet = TRUE)
    
    if (any(is.na(geocode_df$lat)) || any(is.na(geocode_df$long))) {
      stop("Geocoding failed: Unable to find coordinates for the provided location.")
    }
    
    coords <- list(
      lat = geocode_df$lat[1],
      lon = geocode_df$long[1],
      display_name = location
    )
    
    # Create a city-like df with geocoded coordinates
    split_locations <- strsplit(coords$display_name, split = ",")[[1]]
    
    city <- data.frame(
      name = split_locations[1],
      country = split_locations[2],
      lat = coords$lat,
      long = coords$lon,
      population = NA
    )
    
  }, error = function(e) {
    stop("Geocoding error: ", e$message)
  })
  
  return(result)
}
