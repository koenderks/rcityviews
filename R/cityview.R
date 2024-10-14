library(dplyr)
library(ggplot2)
library(osmdata)
library(memoise)

#' Create a City View with Optional Caching
#'
#' @description Create a city view showcasing a particular city or region using
#'   OpenStreetMap (OSM) data retrieved through the Overpass API. The function
#'   supports arbitrary location names by integrating geocoding functionality. 
#'   It includes caching options to avoid redundant data retrieval.
#'
#' @usage cityview(
#'   name = NULL,
#'   zoom = 1,
#'   theme = c(
#'     "vintage", "modern", "bright", "delftware", "comic",
#'     "rouge", "original", "midearth", "batik", "vice", 
#'     "default", "macao", "minimal", "tijuca", "oslo", 
#'     "tokyo", "paris", "lunar_shadow", "urban_glow"
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
#'   bot = FALSE,
#'   method = "osm",
#'   cache = TRUE,
#'   persistent_cache = TRUE,
#'   clear_cache = FALSE
#' )
#'
#' @param name A character specifying the name of the city or location to view.
#' @param zoom A numeric value specifying the zoom level. Higher values increase
#'   the zoom, resulting in a more detailed view.
#' @param theme A character string specifying the plot theme. Options include
#'   "vintage", "modern", "bright", and others. See usage for the complete list.
#' @param border A character specifying the type of border to use. Options are
#'   "none", "circle", "rhombus", "square", "hexagon", "octagon", "decagon", and "bbox".
#' @param halftone Optional character specifying the color of halftone dither.
#' @param legend Logical. If TRUE, adds a distance measure and compass.
#' @param places An integer indicating the number of place names to add to the view.
#' @param license Logical. If TRUE, adds an OpenStreetMap license annotation.
#' @param timeout A numeric value specifying the timeout for Overpass API requests.
#' @param filename Optional character. If specified, saves the output as a file.
#' @param verbose Logical. If TRUE, displays a progress bar and messages.
#' @param bot Logical. If TRUE, enables bot-specific functionality.
#' @param method Character specifying the geocoding service to use (default: "osm").
#' @param cache Logical. If TRUE, enables caching of the OSM data retrieval.
#' @param persistent_cache Logical. If TRUE, uses a persistent cache on disk, otherwise in-memory.
#' @param clear_cache Logical. If TRUE, clears the cache before proceeding.
#'
#' @details This function allows users to create a customizable city view map
#'   using data from OpenStreetMap. The caching mechanism helps to avoid redundant 
#'   data retrieval, speeding up the process when retrieving the same city view multiple times.
#'
#' @return Returns a ggplot2 object representing the city view, or saves it as a
#'   file if filename is specified.
#'
#' @examples
#' \dontrun{
#' # Create a city view of Amsterdam with caching enabled
#' cityview(name = "Amsterdam", theme = "vintage", border = "circle")
#'
#' # Create a city view of Hellebecq, Belgium without using persistent cache
#' cityview(name = "Hellebecq, Belgium", theme = "default", persistent_cache = FALSE)
#'
#' # Clear the cache and generate a new view
#' cityview(name = "Paris", clear_cache = TRUE)
#' }
#' @export
cityview <- function(name = NULL,
                     zoom = 1,
                     theme = c(
                       "vintage", "modern", "bright", "delftware", "comic",
                       "rouge", "original", "midearth", "batik", "vice", 
                       "default", "macao", "minimal", "tijuca", "oslo", 
                       "tokyo", "paris", "lunar_shadow", "urban_glow"
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
                     bot = FALSE,
                     method = "osm",
                     cache = TRUE,
                     persistent_cache = TRUE,
                     clear_cache = FALSE) {
  
  # Theme and border setup
  theme <- match.arg(theme)
  themeOptions <- .themeOptions(theme)
  border <- match.arg(border)
  
  # Persistent cache directory setup
  cache_dir <- file.path(path.expand("~"), "Documents", "cityview_cache")
  if (persistent_cache && !dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  # Handle cache clearing
  if (clear_cache) {
    if (persistent_cache && dir.exists(cache_dir)) {
      unlink(cache_dir, recursive = TRUE)
      message("Persistent cache cleared.")
    } else if (cache) {
      message("In-memory cache cleared.")
    }
  }
  
  # Create cached buildCity function
  buildCity_func <- if (cache) {
    if (persistent_cache) {
      memoise(.buildCity, cache = cache_filesystem(cache_dir))
    } else {
      memoise(.buildCity)
    }
  } else {
    .buildCity
  }
  
  # Geocode the city if necessary
  if (!is.null(name)) {
    city <- .getCity(name)
    if (nrow(city) == 0) {
      if (verbose) message("Geocoding location: ", name)
      city <- geocode_raw(name, method = method)
    }
  } else {
    city <- .getRandomCity()
  }
  
  if (bot) cat(paste0(city[["name"]], ", ", city[["country"]]))
  
  # Retrieve data and bounding box
  boundaries <- .getBoundaries(city = city, border = border, zoom = zoom)
  bbox <- osmdata::opq(bbox = boundaries[["panel"]], timeout = timeout)
  
  # Build the map with or without cache
  image <- buildCity_func(
    city = city, bbox = bbox, zoom = boundaries[["zoom"]],
    panel = boundaries[["panel"]], themeOptions = themeOptions,
    border = border, halftone = halftone, legend = legend,
    places = places, cropped = boundaries[["cropped"]],
    borderPoints = boundaries[["borderPoints"]], license = license,
    verbose = verbose, ticks = 61 + as.numeric(!is.null(halftone)) + as.numeric(places > 0),
    shiny = FALSE
  )
  
  # Return or save the image
  if (is.null(filename)) {
    return(image)
  } else {
    ggplot2::ggsave(filename, plot = image, height = 500, width = 500, units = "mm", dpi = 100)
    return(invisible())
  }
}
