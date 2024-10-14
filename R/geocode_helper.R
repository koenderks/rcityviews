# R/geocode_helper.R

geocode_raw <- function(location, method = "osm") {
  if (missing(location) || !is.character(location)) {
    stop("Please provide a valid location name as a string.")
  }
  
  # Validate geocoding method
  supported_methods <- c("osm", "census", "arcgis", "census_simple", "geocodio",
                         "mapbox", "google", "bing", "here", "tomtom", "nominatim", "tiger")
  if (!(method %in% supported_methods)) {
    stop("Unsupported geocoding method. Please choose one of: ", paste(supported_methods, collapse = ", "))
  }
  
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
    
    coords = list(
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

