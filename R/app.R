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

.shiny_ui <- shiny::fluidPage(
  shiny::tags$head(shiny::HTML("<title>R City Views</title>")),
  shiny::fluidRow(align = "center", shiny::titlePanel(shiny::HTML("<h1><b>R City Views</b></h1>"))),
  shiny::hr(),
  shiny::fluidRow(
    shiny::column(3,
      style = "border: 4px double black;",
      shiny::fluidRow(align = "center", shiny::HTML("<h2>Instructions</h2>")),
      shiny::HTML("<p><b>Step 1:</b> Drag the map, click <span style='color: #edb92e'><b>Randomize</b></span>, or use the search tool to find the area that you want to render.</p>
                     <p><b>Step 2:</b> Adjust the name of the city under <b>City name</b> and the country under <b>Country</b>.</p>
                     <p><b>Step 3:</b> Click the <span style='color: #f85931'><b>Preview</b></span> button to preview the map in the panel below (this may take a while).</p>
                     <p><b>Step 4:</b> Once the image is displayed you can click the <span style='color: #009989'><b>Download</b></span> button to export it as an <b>.svg</b> file.</p>
                     <p><b>Note:</b> Previewing a (too) large or populated area may disconnect you from the server due to a data limit of 1 GB for free Shiny subscriptions. In this case, use <tt>rcityviews::cityview_shiny()</tt> or download <a href='https://raw.githubusercontent.com/koenderks/rcityviews/master/R/app.R' target='_blank'>app.R</a> from GitHub to run on your own computer.</p>"),
    ),
    shiny::column(4,
      offset = 1,
      shiny::fluidRow(align = "center", leaflet::leafletOutput(outputId = "osm", width = "500px", height = "410px"))
    ),
    shiny::column(3,
      offset = 1,
      shiny::fluidRow(align = "center", shiny::textInput(inputId = "plotTitle", label = "City name", value = "")),
      shiny::fluidRow(align = "center", shiny::textInput(inputId = "countryTitle", label = "Country", value = "")),
      shiny::fluidRow(
        align = "center",
        shiny::column(width = 6, shiny::selectInput(inputId = "theme", label = "Theme", choices = c("Vintage", "Modern", "Bright", "Delftware", "Comic", "Rouge", "Original", "Midearth", "Batik", "Vice"))),
        shiny::column(width = 6, shiny::selectInput(inputId = "border", label = "Border", choices = c("None", "Circle", "Rhombus", "Square", "Hexagon", "Octagon", "Decagon")))
      ),
      shiny::fluidRow(
        align = "center",
        shiny::column(width = 6, shiny::checkboxInput(inputId = "legend", label = "Legend", value = FALSE)),
        shiny::column(width = 6, shiny::checkboxInput(inputId = "license", label = "License", value = TRUE))
      ),
      shiny::fluidRow(
        align = "center",
        shiny::actionButton(inputId = "randomize", label = "Randomize", icon = shiny::icon("dice"), style = "color: #000000; background-color: #edb92e; border-color: #000000"),
        shiny::actionButton(inputId = "run", label = "Preview", icon = shiny::icon("camera"), style = "color: #000000; background-color: #f85931; border-color: #000000"),
        shiny::downloadButton(outputId = "downloadPlot", label = "Download", style = "color: #000000; background-color: #009989; border-color: #000000")
      ),
      shiny::HTML("<br>"),
      shiny::fluidRow(
        align = "center",
        shiny::tags$a(href = "https://koenderks.shinyapps.io/rcityviews/", "Tweet", class = "twitter-share-button"),
        shiny::includeScript("http://platform.twitter.com/widgets.js")
      )
    )
  ),
  shiny::hr(),
  shiny::mainPanel(
    width = 12, style = "border: 4px double black;",
    shiny::fluidRow(align = "center", shiny::plotOutput(outputId = "plotObject", width = "1400px", height = "1600px"))
  )
)

.shiny_server <- function(input, output, session) {
  city <- rcityviews:::.randomCity(sample.int(100000, size = 1))
  shiny::updateTextInput(session, "plotTitle", value = city[["name"]])
  shiny::updateTextInput(session, "countryTitle", value = city[["country"]])
  output[["osm"]] <- leaflet::renderLeaflet({
    leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::setView(lng = city[["long"]], lat = city[["lat"]], zoom = 14) |>
      leaflet.extras::addSearchOSM()
  })
  shiny::observeEvent(input[["randomize"]], {
    city <- rcityviews:::.randomCity(sample.int(100000, size = 1))
    shiny::updateTextInput(session, "plotTitle", value = city[["name"]])
    shiny::updateTextInput(session, "countryTitle", value = city[["country"]])
    output[["osm"]] <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::setView(lng = city[["long"]], lat = city[["lat"]], zoom = 14) |>
        leaflet.extras::addSearchOSM()
    })
  })
  output[["plotObject"]] <- shiny::renderPlot(NULL)
  shiny::observeEvent(input[["run"]], {
    themeOptions <- rcityviews:::.themeOptions(tolower(input[["theme"]]))
    long <- stats::median(c(input[["osm_bounds"]][["east"]], input[["osm_bounds"]][["west"]]))
    lat <- stats::median(c(input[["osm_bounds"]][["north"]], input[["osm_bounds"]][["south"]]))
    city <- data.frame("name" = input[["plotTitle"]], "country" = input[["countryTitle"]], lat = lat, long = long)
    boundaries <- rcityviews:::.getBoundaries(city, tolower(input[["border"]]), input = input)
    bbox <- osmdata::opq(bbox = boundaries[["panel"]], timeout = 25)
    try <- try({
      shiny::withProgress(message = "Creating preview", value = 0, min = 0, max = 1, expr = {
        image <- rcityviews:::.buildCity(
          city = city,
          bbox = bbox,
          zoom = 0.0225 / (city[["lat"]] - boundaries[["panel"]][2]),
          panel = boundaries[["panel"]],
          themeOptions = themeOptions,
          border = tolower(input[["border"]]),
          halftone = NULL,
          places = 0,
          legend = input[["legend"]],
          cropped = boundaries[["cropped"]],
          borderPoints = boundaries[["borderPoints"]],
          verbose = FALSE,
          license = input[["license"]],
          ticks = 61,
          shiny = TRUE
        )
      })
    })
    if ("try-error" %in% class(try)) {
      if (try[[1]] == "Error in resp_abort(resp, error_body(req, resp)) : \n  HTTP 504 Gateway Timeout.\n") {
        shiny::showNotification("The overpass server is not able to respond to your request, traffic might be too high.", type = "error", duration = NULL)
      } else {
        shiny::showNotification(try[[1]], type = "error", duration = NULL)
      }
      return()
    }
    output[["plotObject"]] <- shiny::renderPlot(image)
    output[["downloadPlot"]] <- shiny::downloadHandler(
      filename = function() {
        paste0(input[["plotTitle"]], ".svg")
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = image, height = 500, width = 500, units = "mm")
      }
    )
  })
}

shiny::shinyApp(ui = .shiny_ui, server = .shiny_server)
