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

#' Persistent Storage of Custom Themes
#'
#' @description Conveniently store custom themes across R sessions using a
#'   persistent caching mechanism. This function creates a cache directory to
#'   save the themes as individual RDS files, and asks permission to the user
#'   before writing.
#'
#'
#' @usage city_themes(name,
#'             theme = NULL,
#'             force = FALSE,
#'             remove = FALSE)
#'
#' @param name   a character vector of the theme to save or extract.
#' @param theme  a list containing theme options to store.
#' @param force  logical. Whether to overwrite a theme if it already exists.
#' @param remove logical. Whether to remove the theme from the cache.
#'
#' @return A list of class \code{rcityviewsTheme} containing the theme options.
#'
#' @author Koen Derks, \email{koen-derks@hotmail.com}
#'
#' @seealso \code{\link{cityview}}
#'          \code{\link{cityview_shiny}}
#'          \code{\link{new_city}}
#'          \code{\link{list_cities}}
#'
#' @keywords cities theme store
#'
#' @examples
#' \dontrun{
#' # Custom theme (black, beige and white), streets only
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
#' # Store the custom theme in the cache
#' city_themes("blackandyellow", theme = myTheme)
#' # Retreive the custom theme from the cache
#' city_themes("blackandyellow")
#' # Remove the custom theme from the cache
#' city_themes("blackandyellow", remove = TRUE)
#' }
#' @export

city_themes <- function(name,
                        theme = NULL,
                        force = FALSE,
                        remove = FALSE) {
  predefined_themes <- c(
    "vintage", "modern", "bright", "delftware", "comic",
    "rouge", "original", "midearth", "batik", "vice"
  )
  if (name %in% predefined_themes) {
    theme <- .themeOptions(name)
  } else {
    cache_location <- file.path(rappdirs::user_cache_dir("rcityviews"), "themes")
    if (!is.null(theme) && interactive()) {
      response <- utils::menu(choices = c("Yes", "No"), title = paste0("Do you give rcityviews permission to write the persistent cache at ", cache_location), "?")
      if (response == 2) {
        .checkThemeOptions(theme)
        class(theme) <- c("rcityviewsTheme", "list")
        attr(theme, "name") <- name
        return(theme)
      }
    }
    if (!dir.exists(cache_location)) {
      dir.create(cache_location, recursive = TRUE)
    }
    existing_theme_files <- list.files(cache_location)
    existing_themes <- sub("_.*", "", existing_theme_files)
    theme_file <- file.path(cache_location, paste0(name, "_theme.rds"))
    theme_exists <- name %in% existing_themes
    if (theme_exists) {
      if (remove) {
        file.remove(theme_file)
        remaining_themes <- existing_themes[-which(existing_themes == name)]
        if (length(remaining_themes) == 0) {
          message(paste0("theme '", name, "' removed from cache, no themes remaining in cache"))
        } else {
          message(paste0("theme '", name, "' removed from cache, remaining themes in cache:\n  "), paste0(remaining_themes, collapse = "\n  "))
        }
        return(invisible())
      } else if (is.null(theme)) {
        theme <- readRDS(theme_file)
      } else {
        if (!force) {
          stop(paste0("theme '", name, "' already exists in cache, use 'force = TRUE' to overwrite"))
        } else {
          stopifnot("the 'theme' argument requires a non-null list input" = !is.null(theme) && is.list(theme))
          .checkThemeOptions(theme)
          saveRDS(theme, theme_file)
          message(paste0("theme '", name, "' overwritten in cache, currently contains ", length(existing_themes), " theme(s)"))
        }
      }
    } else {
      if (is.null(theme)) {
        if (length(existing_themes) == 0) {
          message(paste0("theme '", name, "' not found, please provide the 'theme' argument"))
        } else {
          message(paste0("theme '", name, "' not found, please provide the 'theme' argument or choose an existing theme from the cache:\n  "), paste0(existing_themes, collapse = "\n  "))
        }
        return(invisible())
      }
      stopifnot("the 'theme' argument requires a list input" = is.list(theme))
      .checkThemeOptions(theme)
      saveRDS(theme, theme_file)
      message(paste0("theme '", name, "' added to cache, currently contains ", length(existing_themes) + 1, " theme(s)"))
    }
  }
  class(theme) <- c("rcityviewsTheme", "list")
  attr(theme, "name") <- name
  return(theme)
}
