# Copyright (C) 2020-2023 Koen Derks

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

#' Methods for rcityviews Objects
#'
#' Methods defined for objects returned from the \code{\link{new_city}} and
#'   \code{\link{city_themes}} functions.
#'
#' @param x    an object of class \code{rcityviewsCity} or
#'             \code{rcityviewsTheme}.
#' @param ...  further arguments, currently ignored.
#'
#' @return
#' The \code{print} methods simply print and return nothing.
#'
#' @name rcityviews-methods
NULL

# Methods for class: rcityviewsCity ############################################

#' @rdname rcityviews-methods
#' @method print rcityviewsCity
#' @export
print.rcityviewsCity <- function(x, ...) {
  cat("\n")
  cat(strwrap(paste0("rcityviews city '", x[["name"]], "'"), prefix = "\t"), sep = "\n")
  cat("\n")
  cat("Details:\n")
  cat("  Name:\t\t", x[["name"]], "\n")
  cat("  Country:\t", x[["country"]], "\n")
  cat("  Latitude:\t", paste0(x[["lat"]], "\u00B0"), "\n")
  cat("  Longitude:\t", paste0(x[["long"]], "\u00B0"))
}

# Methods for class: rcityviewsTheme ###########################################

#' @rdname rcityviews-methods
#' @method print rcityviewsTheme
#' @export
print.rcityviewsTheme <- function(x, ...) {
  cat("\n")
  cat(strwrap(paste0("rcityviews theme '", attr(x, "name"), "'"), prefix = "\t"), sep = "\n")
  cat("\n")
  cat("Details:\n")
  cat("  Colors:\n")
  cat("    Background:\t\t", x[["colors"]][["background"]], "\n")
  cat("    Water:\t\t", x[["colors"]][["water"]], "\n")
  cat("    Landuse:\t\t", paste0(x[["colors"]][["landuse"]], collapse = " + "), "\n")
  cat("    Contours:\t\t", x[["colors"]][["contours"]], "\n")
  cat("    Streets:\t\t", x[["colors"]][["streets"]], "\n")
  cat("    Rails:\t\t", paste(x[["colors"]][["rails"]], collapse = " + "), "\n")
  cat("    Buildings:\t\t", paste(x[["colors"]][["buildings"]], collapse = " + "), "\n")
  cat("    Text:\t\t", x[["colors"]][["text"]], "\n")
  cat("    Waterlines:\t\t", x[["colors"]][["waterlines"]], "\n")
  cat("  Font:\n")
  cat("    Family:\t\t", x[["font"]][["family"]], "\n")
  cat("    Face:\t\t", x[["font"]][["face"]], "\n")
  cat("    Scale:\t\t", x[["font"]][["scale"]], "\n")
  cat("    Append:\t\t", x[["font"]][["append"]], "\n")
  cat("  Size:\n")
  cat("    Borders:\n")
  cat("      Contours:\t\t", x[["size"]][["borders"]][["contours"]], "\n")
  cat("      Water:\t\t", x[["size"]][["borders"]][["water"]], "\n")
  cat("      Canal:\t\t", x[["size"]][["borders"]][["canal"]], "\n")
  cat("      River:\t\t", x[["size"]][["borders"]][["river"]], "\n")
  cat("    Streets:\n")
  cat("      Path:\t\t", x[["size"]][["streets"]][["path"]], "\n")
  cat("      Residential:\t", x[["size"]][["streets"]][["residential"]], "\n")
  cat("      Structure:\t", x[["size"]][["streets"]][["structure"]], "\n")
  cat("      Tertiary:\t\t", x[["size"]][["streets"]][["tertiary"]], "\n")
  cat("      Secondary:\t", x[["size"]][["streets"]][["secondary"]], "\n")
  cat("      Primary:\t\t", x[["size"]][["streets"]][["primary"]], "\n")
  cat("      Motorway:\t\t", x[["size"]][["streets"]][["motorway"]], "\n")
  cat("      Rails:\t\t", x[["size"]][["streets"]][["rails"]], "\n")
  cat("      Runway:\t\t", x[["size"]][["streets"]][["runway"]])
}
