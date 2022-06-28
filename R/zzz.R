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
  sysfonts::font_add_google("Neonderthaw")
  sysfonts::font_add_google("Oswald")
  sysfonts::font_add_google("Righteous")
  showtext::showtext_auto()
}

utils::globalVariables(c("x", "y"))
