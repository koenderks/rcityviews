[![R_build_status](https://github.com/koenderks/rcityviews/workflows/Build/badge.svg)](https://github.com/koenderks/rcityviews/actions)
[![Bugs](https://img.shields.io/github/issues/koenderks/rcityviews/bug?label=Bugs&logo=github&logoColor=%23FFF&color=brightgreen)](https://github.com/koenderks/rcityviews/issues?q=is%3Aopen+is%3Aissue+label%3Abug)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

<p align="center">
  <img src='https://github.com/koenderks/rcityviews/raw/development/png/Banner.png' width='100%'>
</p>

# R City Views

<img src='https://github.com/koenderks/rcityviews/raw/development/man/figures/logo.png' width='149' height='173' alt='logo' align='right' margin-left='20' margin-right='20'/>

This repository is an homage to the programming language `R`, open-source
geographic data and the art of map making. It provides code and examples to
render customizable stylized city maps using data from
[OpenStreetMap](https://www.openstreetmap.org/). Take a look at the
[tutorial](https://koenderks.github.io/rcityviews/articles/rcityviews.html) for
a quick guide on how to get started.

* [Installation](#installation)
* [Create your own in R](#create-your-own-in-r)
* [Create your own in Shiny](#create-your-own-in-shiny)
* [Acknowledgements](#acknowledgements)

Every three hours this repository creates and tweets a view of a random city.
You can find all city views created so far at the twitter handle
[`@rcityviews`](https://twitter.com/rcityviews). Please do not hesitate to share
your own creations using the hashtag `#rcityviews`!

## Installation

The functionality in this repository is implemented in the `R` package
`rcityviews`. This package is not available on CRAN but can be obtained via
GitHub by running the command below in `R`.

```r
# install.packages("remotes") # Uncomment if 'remotes' package is not installed
remotes::install_github("koenderks/rcityviews", dependencies = TRUE)
```

After installation, you can load the package into the `R` session using the
following command.

```r
library(rcityviews)
```

## Create your own in R

### Finding a city to map

First, you can search for a city name in the package database using the
`list_cities()` function. This function looks in the internal database and finds
any city name that contains the expression in `match`.

```r
list_cities(match = "Ams")
#>                  name         country   lat   long
#> 1356       Amstelveen The Netherlands 52.32   4.86
#> 1357        Amsterdam The Netherlands 52.37   4.89
#> 1358        Amstetten         Austria 48.13  14.86
#> 25857   New Amsterdam          Guyana  6.25 -57.53
#> 26031 Nieuw Amsterdam        Suriname  5.91 -55.07
```

If you cannot find your preferred city in the internal package database, you can
use the `new_city()` function to manually specify a location using its latitude
and longitude coordinates.

```r
city <- new_city(name = "Lagos", country = "Portugal", lat = 37.10, long = -8.68)
#> Discovered the city of Lagos, Portugal at 37.1° / -8.68°!
```

### Creating the map

Second, once you have obtained the name of the city you want to view or have
specified a location of a city, you can use the `cityview()` function to create
a `ggplot2` object. Use the `zoom` argument to zoom in on your city (e.g.,
`zoom > 1`, decreases computation time) or zoom out of your city (e.g.,
`zoom < 0.5`, increases computation time). By default, `cityview()` is called
with the `cache = TRUE` flag, which means that it will cache the map data so
that you can quickly try out different themes (see below).

```r
p <- cityview(name = "Amsterdam", zoom = 1) # or cityview(name = city)
# see ?cityview for more input parameters of this function
```

### Saving the map

Finally, render times in `R` or RStudio can be very long for crowded spatial
images. It is therefore recommended to directly save the image in a
`500mm x 500mm` format. Typically, the ideal way to do this given a `ggplot2`
object named `p` is to execute the command below.

```r
ggplot2::ggsave(filename = "Amsterdam.png", plot = p, height = 500, width = 500, units = "mm", dpi = 100)
```

However, you can also do this instantly by providing a filename directly to the
`cityview()` function via its `filename` argument. To save rendering time, the
image is exported in an appropriate size and the function does not return a
`ggplot2` object.

```r
cityview(name = "Amsterdam", filename = "Amsterdam.png")
```

<p align="center">
  <img src='https://github.com/koenderks/rcityviews/raw/development/png/Amsterdam.png' width='100%'>
</p>

For personal (non-commercial) printing it is advised to use the option
`license = FALSE` and save the image to a `.pdf` or `.svg` file. Afterwards, the
image is best printed in a `500mm x 500mm` format.

### Styling the map

There are ten pre-specified themes that can be used to style the image. The
image above is created using `theme = "vintage"` (the default), but other
options for the `theme` argument include `modern` (top left), `bright` (top
middle), `delftware` (top right), `comic` (middle left), `rouge` (middle
middle), `original` (middle right), `midearth` (bottom left), `batik` (bottom
middle) and `vice` (bottom right).

<p align="center">
  <img src='https://github.com/koenderks/rcityviews/raw/development/png/Osaka.png' width='30%'>
  <img src='https://github.com/koenderks/rcityviews/raw/development/png/Marseille.png' width='30%'>
  <img src='https://github.com/koenderks/rcityviews/raw/development/png/NewYork.png' width='30%'>
  <br>
  <img src='https://github.com/koenderks/rcityviews/raw/development/png/Florence.png' width='30%'>
  <img src='https://github.com/koenderks/rcityviews/raw/development/png/Madrid.png' width='30%'>
  <img src='https://github.com/koenderks/rcityviews/raw/development/png/SanFrancisco.png' width='30%'>
  <br>
  <img src='https://github.com/koenderks/rcityviews/raw/development/png/Tokyo.png' width='30%'>
  <img src='https://github.com/koenderks/rcityviews/raw/development/png/London.png' width='30%'>
  <img src='https://github.com/koenderks/rcityviews/raw/development/png/Adelaide.png' width='30%'>
</p>

In addition to the ten pre-specified themes, the package provides full
flexibility to customize the theme by providing a named list. This is
demonstrated in the code block below.

```r
# For example: black, beige and white theme, streets only
myTheme <- list(
  colors = list(
    background = "#232323",
    water = "#232323",
    landuse = "#232323",
    contours = "#232323",
    streets = "#d7b174",
    rails = c("#d7b174", "#232323"),
    buildings = "#232323",
    text = "#ffffff",
    waterlines = "#232323"
  ),
  font = list(
    family = "serif",
    face = "bold",
    scale = 1,
    append = "\u2014"
  ),
  size = list(
    borders = list(
      contours = 0.15,
      water = 0.4,
      canal = 0.5,
      river = 0.6
    ),
    streets = list(
      path = 0.2,
      residential = 0.3,
      structure = 0.35,
      tertiary = 0.4,
      secondary = 0.5,
      primary = 0.6,
      motorway = 0.8,
      rails = 0.65,
      runway = 3
    )
  )
)
cityview(name = "Rio de Janeiro", zoom = 0.5, theme = myTheme, border = "square", filename = "Rio.png")
```

<p align="center">
  <img src='https://github.com/koenderks/rcityviews/raw/development/png/Rio.png' width='100%'>
</p>

You can store the custom theme in the package cache for retrieval in a future R
session with the `city_themes()` function. This is illustrated below for
the `myTheme` list.

```r
# Store the theme in the persistent cache
city_themes(name = "blackyellow", theme = myTheme)
# Retreive the theme from the persistent cache (e.g., in a future R session)
city_themes(name = "blackyellow")
# Remove the theme from the persistent cache
city_themes(name = "blackyellow", remove = TRUE)
```

To use a custom font in `myTheme[["font"]][["family"]]`, simply donwload a
`.ttf` file of the font from the web, save it as `path/to/font/<font_name>.ttf`
and register the font via the code below. Then, use `<font_name>` for `family`.

```r
sysfonts::font_add("<font_name>", "path/to/font/<font_name>.ttf")
```

### Enclosing the map

There are several types of borders that can be used to enclose the city. The
image above is created using `border = "square"`, but other options for the
`border` argument include `none` (the default), `circle` (left), `rhombus`
(middle), `square`, `hexagon`, `octagon`, `decagon` and `bbox` (right).

<p align="center">
  <img src='https://github.com/koenderks/rcityviews/raw/development/png/Venice.png' width='30%'>
  <img src='https://github.com/koenderks/rcityviews/raw/development/png/Dublin.png' width='30%'>
  <img src='https://github.com/koenderks/rcityviews/raw/development/png/Lisbon.png' width='30%'>
</p>

### Other display options

There are three other arguments to the `cityview()` function that can be used to
tailor the image to your liking. First, the argument `halftone` allows you to
add a colored dotted dither to the image (e.g., `halftone = "#ffffff"`, left).
Second, setting `legend = TRUE` adds a distance measurer and a compass to the
image (middle). Third, the argument `places` takes an integer and adds that
amount of names of towns, villages, suburbs, quarters and neighbourhoods to the
image (e.g., `places = 10`, right).

<p align="center">
  <img src='https://github.com/koenderks/rcityviews/raw/development/png/HaNoi.png' width='30%'>
  <img src='https://github.com/koenderks/rcityviews/raw/development/png/Glasgow.png' width='30%'>
  <img src='https://github.com/koenderks/rcityviews/raw/development/png/Budapest.png' width='30%'>
</p>

## Create your own in Shiny

You can make your own images without having to code using an `R Shiny`
implementation of the package. A live version of the application can be found
[here](https://koenderks.shinyapps.io/rcityviews/) but it is also easily
accessible from within `R` by calling the function `cityview_shiny()`.

<p align="center">
  <img src='https://github.com/koenderks/rcityviews/raw/development/png/app.png' width='100%'>
</p>

## Acknowledgements

The data is available under the [Open Database License](https://www.openstreetmap.org/copyright).
