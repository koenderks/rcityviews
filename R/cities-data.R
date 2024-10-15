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

#' Geographical Locations of World Cities
#'
#' Data set with geographical locations of cities. These data can also be found
#'   in the \code{maps} package.
#'
#' @docType data
#'
#' @usage data(cities)
#'
#' @format A data frame with 43645 rows and 5 variables.
#' \describe{
#'   \item{name}{name of the city}
#'   \item{country}{recorded values for entity 1, in US dollars.}
#'   \item{population}{recorded values for entity 2, in US dollars.}
#'   \item{lat}{recorded values for entity 3, in US dollars.}
#'   \item{long}{recorded values for entity 4, in US dollars.}
#' }
#'
#' @keywords datasets
#'
#' @source \url{https://cran.r-project.org/package=maps}
#'
#' @examples
#' data(cities)
"cities"
