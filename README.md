[![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)](https://github.com/koenderks/rcityviews/tree/master/R)
[![Twitter](https://img.shields.io/badge/Twitter-1DA1F2?style=for-the-badge&logo=twitter&logoColor=white)](https://twitter.com/rcityviews)
[![R_build_status](https://github.com/koenderks/rcityviews/workflows/Build/badge.svg)](https://github.com/koenderks/rcityviews/actions)
[![Bugs](https://img.shields.io/github/issues/koenderks/rcityviews/bug?label=Bugs&logo=github&logoColor=%23FFF&color=brightgreen)](https://github.com/koenderks/rcityviews/issues?q=is%3Aopen+is%3Aissue+label%3Abug)

# R City Views

<img src='https://github.com/koenderks/rcityviews/raw/master/man/figures/logo.png' width='149' height='173' alt='logo' align='right' margin-left='20' margin-right='20'/>

This repository is an homage to the programming language `R`, open-source geographic data and the art of map making. It provides code and examples to generate minimalistic aerial city views using data from [OpenStreetMap](https://www.openstreetmap.org/).

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

The functionality in this repository is implemented as an `R` package: `rcityviews`. However, the `rcityviews` package is not available on CRAN. It can therefore only be obtained via GitHub by running the following command in `R`:

```r
# install.packages("remotes") # Uncomment if you do not have the 'remotes' package installed
remotes::install_github("koenderks/rcityviews", dependencies = TRUE)
```

After installation, the package can be loaded with:

```r
library(rcityviews)
```

## Create your own in R

### Finding a city name in the database

First, you can search for a city name in the database using the `list_cities()` function. This function searches the database and finds any match for the expression in `match`.

```r
list_cities("Ams")
# [1] "Amstelveen" "Amsterdam" "Amstetten" "New Amsterdam" "Nieuw Amsterdam"
```

### Creating the image

Second, once you have obtained the name of the city you want to view, you can use the `cityview()` function to create a `ggplot2` object. Use the `zoom` argument to zoom in on your city (e.g., to speed up computation time).

```r
p <- cityview(name = "Amsterdam", zoom = 1, border = "circle")
# see ?cityview for more input parameters of this function
```

### Saving the image

Finally, render times in `RStudio` can be very long for these type of plots. It is therefore recommended to directly save the images in a `500mmx500mm` format. The ideal way to do this is usually something like:

```r
ggplot2::ggsave(filename = "Amsterdam.png", plot = p, height = 500, width = 500, units = "mm", dpi = 100)
```

However, you can also do this instantly by providing a filename directly to the `cityview()` function. This way, the image is exported in an appropriate size and the function does not return a `ggplot2` object to save rendering time.

```r
cityview(name = "Amsterdam", border = "circle", filename = "Amsterdam.png")
```

<p align="center">
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/Amsterdam_circle_border.png' width='400' height='400'>
</p>

For printing, it is recommended to use the option `license = FALSE` and save the image to a `.pdf` or `.svg` file, see below. Afterwards, you can best print it in a `50x50` cm format.

```r
cityview(name = "Amsterdam", border = "circle", filename = "Amsterdam.pdf", license = FALSE)
```

### Border styles

You can select different types of borders to enclose the city. The image above is created using `border = "circle"`, but other options for the `border` argument include `none` (left), `rhombus` (middle), `square`, `hexagon`, `octagon`, and `decagon` (right).

<p align="center">
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/Amsterdam_no_border.png' width='250' height='250'>
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/Amsterdam_rhombus_border.png' width='250' height='250'>
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/Amsterdam_decagon_border.png' width='250' height='250'>
</p>

### Themes

You can select different themes for the plot. The images above are created using `theme = "original"`, but other options for the `theme` argument include `light` (top left), `dark` (bottom left), `rouge` (top middle), `verde` (bottom middle), `colored` (top right), `neon` (bottom right), and `lichtenstein`.

<p align="center">
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/Amsterdam_light.png' width='250' height='250'>
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/Amsterdam_rouge.png' width='250' height='250'>
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/Amsterdam_colored.png' width='250' height='250'>
  </br>
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/Amsterdam_dark.png' width='250' height='250'>
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/Amsterdam_verde.png' width='250' height='250'>
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/Amsterdam_neon.png' width='250' height='250'>
</p>

## Create your own in Shiny

You can easily make your own custom images using an `R` Shiny implementation of the package. The live application can be found [here](https://koenderks.shinyapps.io/rcityviews/) and the code to run the app locally can be found [here](https://raw.githubusercontent.com/koenderks/rcityviews/master/app.R).

<p align="center">
  <img src='https://github.com/koenderks/rcityviews/raw/master/png/app.png' width='900' height='300'>
</p>

## Acknowledgements

The data is available under the [Open Database License](https://www.openstreetmap.org/copyright).
