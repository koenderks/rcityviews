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

.onLoad <- function(libname, pkgname) {
  sysfonts::font_add_google("Caveat")
  sysfonts::font_add_google("Imbue")
  sysfonts::font_add_google("Damion")
  sysfonts::font_add_google("Oswald")
  sysfonts::font_add_google("Rampart One")
  sysfonts::font_add_google("Fredericka the Great")
  sysfonts::font_add_google("Dancing Script")
  sysfonts::font_add_google("Walter Turncoat")
  sysfonts::font_add("American Uncial Regular", system.file("fonts", "uncial.otf", package = "rcityviews"))
  sysfonts::font_add("Rage", system.file("fonts", "rage.ttf", package = "rcityviews"))
  showtext::showtext_auto()
  osmdata::set_overpass_url("https://overpass.kumi.systems/api/interpreter")
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Data by \u00A9 OpenStreetMap contributors")
}

utils::globalVariables(c("x", "y", "name"))
