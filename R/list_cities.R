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

#' List or Search City Names
#'
#' @description List all city names.
#'
#'
#' @usage list_cities(match = NULL)
#'
#' @param match       string to match.
#'
#' @author Koen Derks, \email{koen-derks@hotmail.com}
#'
#' @seealso \code{\link{cityview}}
#'
#' @keywords cities search
#'
#' @examples
#' # List all cities containing "Ams"
#' list_cities(match = "Ams")
#' @export

list_cities <- function(match = NULL) {
  cities <- rcityviews::cities
  names <- cities[["name"]]
  if (!is.null(match)) {
    names <- grep(match, names, value = TRUE)
  }
  return(names)
}
