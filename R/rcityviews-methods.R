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

#' Methods for rcityviews objects
#'
#' Methods defined for objects returned from the \code{\link{city_themes}}
#'   functions.
#'
#' @param x    an object of class \code{rcityviewsTheme}.
#' @param ...  further arguments, currently ignored.
#'
#' @return
#' The \code{print} methods simply print and return nothing.
#'
#' @name rcityviews-methods
NULL

# Methods for class: rcityviewsTheme ###########################################

#' @rdname rcityviews-methods
#' @method print rcityviewsTheme
#' @export
print.rcityviewsTheme <- function(x, ...) {
  cat("\n")
  cat(strwrap(paste0("rcityviews theme '", attr(x, "name"), "'"), prefix = "\t"), sep = "\n")
  cat("\n")
  cat("Theme:\n")
  cat("  Colors:\n")
  cat("    Background:", x[["colors"]][["background"]], "\n")
  cat("    Water:", x[["colors"]][["water"]], "\n")
  cat("    Landuse:", x[["colors"]][["landuse"]], "\n")
  cat("    Contours:", x[["colors"]][["contours"]], "\n")
  cat("    Streets:", x[["colors"]][["streets"]], "\n")
  cat("    Rails:", x[["colors"]][["rails"]], "\n")
  cat("    Buildings:", x[["colors"]][["buildings"]], "\n")
  cat("    Text:", x[["colors"]][["text"]], "\n")
  cat("    Waterlines:", x[["colors"]][["waterlines"]], "\n")
  cat("  Font:\n")
  cat("    Family:", x[["font"]][["family"]], "\n")
  cat("    Face:", x[["font"]][["face"]], "\n")
  cat("    Scale:", x[["font"]][["scale"]], "\n")
  cat("    Append:", x[["font"]][["append"]], "\n")
  cat("  Size:\n")
  cat("    Borders:\n")
  cat("      Contours:", x[["size"]][["borders"]][["contours"]], "\n")
  cat("      Water:", x[["size"]][["borders"]][["water"]], "\n")
  cat("      Canal:", x[["size"]][["borders"]][["canal"]], "\n")
  cat("      River:", x[["size"]][["borders"]][["river"]], "\n")
  cat("    Streets:\n")
  cat("      Path:", x[["size"]][["streets"]][["path"]], "\n")
  cat("      Residential:", x[["size"]][["streets"]][["residuential"]], "\n")
  cat("      Structure:", x[["size"]][["streets"]][["structure"]], "\n")
  cat("      Tertiary:", x[["size"]][["streets"]][["tertiary"]], "\n")
  cat("      Secondary:", x[["size"]][["streets"]][["secondary"]], "\n")
  cat("      Primary:", x[["size"]][["streets"]][["primary"]], "\n")
  cat("      Motorway:", x[["size"]][["streets"]][["motorway"]], "\n")
  cat("      Rails:", x[["size"]][["streets"]][["rails"]], "\n")
  cat("      Runway:", x[["size"]][["streets"]][["runway"]], "\n")
}
