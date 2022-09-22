[![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)](https://github.com/koenderks/rcityviews/tree/master/R)
[![Twitter](https://img.shields.io/badge/Twitter-1DA1F2?style=for-the-badge&logo=twitter&logoColor=white)](https://twitter.com/rcityviews)
[![R_build_status](https://github.com/koenderks/rcityviews/workflows/Build/badge.svg)](https://github.com/koenderks/rcityviews/actions)
[![Bugs](https://img.shields.io/github/issues/koenderks/rcityviews/bug?label=Bugs&logo=github&logoColor=%23FFF&color=brightgreen)](https://github.com/koenderks/rcityviews/issues?q=is%3Aopen+is%3Aissue+label%3Abug)

# R City Views

<img src='https://github.com/koenderks/rcityviews/raw/master/man/figures/logo.png' width='149' height='173' alt='logo' align='right' margin-left='20' margin-right='20'/>

This repository is an homage to the programming language `R`, open-source geographic data and the art of map making. It provides code and examples to generate minimalistic city views using data from [OpenStreetMap](https://www.openstreetmap.org/).

* [Latest city view](#latest-city-view)
* [Installation](#installation)
* [Create your own in R](#create-your-own-in-r)
* [Create your own in Shiny](#create-your-own-in-shiny)
* [Acknowledgements](#acknowledgements)

## Latest city view

Every 3 hours this repository creates and tweets a view of a random city. You can find all city views created so far at the twitter handle [@rcityviews](https://twitter.com/rcityviews). This is the latest city view:

<p align="center">
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/daily.png' width='400' height='400'>
</p>

## Installation

The functionality in this repository is implemented as an `R` package: `rcityviews`. However, this package is not available on CRAN and can therefore only be obtained via GitHub by running the following command in `R`:

```r
# install.packages("remotes") # Uncomment if you do not have the 'remotes' package installed
remotes::install_github("koenderks/rcityviews", dependencies = TRUE)
```

After installation, the `rcityviews` package can be loaded with:

```r
library(rcityviews)
```

## Create your own in R

### Finding a city name in the database

First, you can search for a city name in the database using the `list_cities()` function. This function searches the database and finds any match for the expression in `match`.

```r
list_cities(match = "Ams")
# [1] "Amstelveen" "Amsterdam" "Amstetten" "New Amsterdam" "Nieuw Amsterdam"
```

### Creating the image

Second, once you have obtained the name of the city you want to view, you can use the `cityview()` function to create a `ggplot2` object. Use the `zoom` argument to zoom in on your city (e.g., `zoom > 1`, speeds up computation time) or zoom out of your city (e.g., `zoom < 0.5`, no buildings will show up on the image).

```r
p <- cityview(name = "Amsterdam", zoom = 1)
# see ?cityview for more input parameters of this function
```

### Saving the image

Finally, render times in `RStudio` can be very long for crowded images. It is therefore recommended to directly save the image in a `500mm x 500mm` format. The ideal way to do this is usually something like:

```r
ggplot2::ggsave(filename = "Amsterdam.png", plot = p, height = 500, width = 500, units = "mm", dpi = 100)
```

However, you can also do this instantly by providing a filename directly to the `cityview()` function. To save rendering time, the image is exported in an appropriate size and the function does not return a `ggplot2` object.

```r
cityview(name = "Amsterdam", filename = "Amsterdam.png")
```

<p align="center">
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/Amsterdam.png' width='400' height='400'>
</p>

For personal (non-commercial) printing it is recommended to use the option `license = FALSE` and save the image to a `.pdf` or `.svg` file, as shown below. Afterwards, the image is best printed in a `500mm x 500mm` format.

```r
cityview(name = "Amsterdam", filename = "Amsterdam.pdf", license = FALSE)
```

### Themes

You can select different pre-specified themes for the image. The image above is created using `theme = "vintage"` (the default), but other options for the `theme` argument include `modern` (top left), `bright` (top middle), `delftware` (top right), `comic` (bottom left), `rouge` (bottom middle) and `original` (bottom right).

<p align="center">
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/Osaka.png' width='250' height='250'>
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/Marseille.png' width='250' height='250'>
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/NewYork.png' width='250' height='250'>
  <br>
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/Florence.png' width='250' height='250'>
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/Madrid.png' width='250' height='250'>
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/SanFrancisco.png' width='250' height='250'>
</p>

However, the package offers full flexibility to specify a custom theme using a named list. This is demonstrated in the code below.

```r
# For example: Green and white theme, streets only
myTheme <- list(
  colors = list(
    background = "forestgreen",
    water = NA,
    landuse = NA,
    contours = NA,
    streets = "white",
    rails = c("white", "forestgreen"),
    buildings = NA,
    text = "white",
    waterlines = NA
  ),
  font = list(
    family = "serif",
    face = "bold"
  ),
  size = list(
    borders = list(
      contours = 0.3,
      water = 0.4,
      canal = 0.5,
      river = 0.6
    ),
    streets = list(
      path = 0.1,
      residential = 0.4,
      structure = 0.5,
      tertiary = 0.75,
      secondary = 0.8,
      primary = 0.9,
      motorway = 1,
      rails = 0.75,
      runway = 3
    )
  )
)
cityview(name = "Rio de Janeiro", theme = myTheme, filename = "Rio.png")
```

<p align="center">
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/Rio.png' width='400' height='400'>
</p>

### Borders

You can select different types of borders to enclose the city. The image above is created using `border = "none"` (the default), but other options for the `border` argument include `circle` (left), `rhombus` (middle), `square`, `hexagon`, `octagon`, and `decagon` (right).

<p align="center">
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/Venice.png' width='250' height='250'>
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/Dublin.png' width='250' height='250'>
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/London.png' width='250' height='250'>
</p>

### Other display options

There are three other arguments to the `cityview()` function that you can use to tailor the image to your liking. First, the argument `halftone` allows you to add a colored dotted dither to the image (e.g., `halftone = "#ffffff"`, left). Second, setting `legend = TRUE` adds a distance measurer and a compass to the image (middle). Third, the argument `places` takes an integer and adds that amount of names of towns, villages, suburbs, quarters and neighbourhoods to the image (e.g., `places = 10`, right).

<p align="center">
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/HaNoi.png' width='250' height='250'>
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/Glasgow.png' width='250' height='250'>
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/Budapest.png' width='250' height='250'>
</p>

## Create your own in Shiny

You can make your own images without having to code using an `R Shiny` implementation of the package. A live version of the application can be found [here](https://koenderks.shinyapps.io/rcityviews/) but it is also easily accessible from within `R` by calling the function `cityview_shiny()`.

<p align="center">
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/app.png' width='900' height='300'>
</p>

## Acknowledgements

The data is available under the [Open Database License](https://www.openstreetmap.org/copyright).
