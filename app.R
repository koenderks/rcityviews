# 1. Packages
require(shiny)
require(shinythemes)
require(shinyjs)
require(ggplot2)
require(osmdata)
require(sf)
require(cowplot)
require(sysfonts)
require(showtext)
require(colourpicker)
require(leaflet)
require(leaflet.extras)
require(svglite)
require(ggspatial)
require(geosphere)
require(maps)
require(stats)

# 2. Preparatory work
sysfonts::font_add_google("Caveat")
showtext_auto()

# 3. Additional functions other than ui and server
draw_plot <- function(input) {
  shiny::withProgress(message = "Creating preview", value = 0, min = 0, max = 1, expr = {
    longitude <- stats::median(c(input$osm_bounds$east, input$osm_bounds$west))
    latitude <- stats::median(c(input$osm_bounds$north, input$osm_bounds$south))
    radius <- geosphere::distm(x = c(longitude, latitude), y = c(longitude, input$osm_bounds$north), fun = geosphere::distHaversine)
    cropped <- data.frame(lat = latitude, long = longitude) |>
      sf::st_as_sf(coords = c("long", "lat"), crs = 4326) |>
      sf::st_buffer(dist = radius)
    newbox <- lapply(sf::st_geometry(cropped), sf::st_bbox)[[1]]
    borders <- circle_border(long = longitude, lat = latitude, rlong = (abs(newbox$xmax) - abs(newbox$xmin)) / 2 * 1.0075, rlat = (abs(newbox$ymax) - abs(newbox$ymin)) / 2 * 1.0075)
    box <- c(newbox$xmin, newbox$ymin, newbox$xmax, newbox$ymax)
    if (input$border == "Rhombus") {
      borders <- data.frame(
        x = c(box[1], longitude, box[3], longitude, box[1]),
        y = c(latitude, box[2], latitude, box[4], latitude)
      )
      cropped <- sf::st_sf(sf::st_sfc(sf::st_polygon(list(as.matrix(borders)))), crs = 4326)
    } else if (input$border == "Square") {
      borders <- data.frame(
        x = c(box[1], box[1], box[3], box[3], box[1]),
        y = c(box[2], box[4], box[4], box[2], box[2])
      )
      cropped <- sf::st_sf(sf::st_sfc(sf::st_polygon(list(as.matrix(borders)))), crs = 4326)
    } else if (input$border == "Hexagon" | input$border == "Octagon" | input$border == "Decagon") {
      nsides <- switch(input$border,
        "Hexagon" = 6,
        "Octagon" = 8,
        "Decagon" = 10
      )
      borders <- data.frame(
        x = longitude + (abs(box[3]) - abs(box[1])) / 2 * cos(2 * pi * 0:nsides / nsides),
        y = latitude + (abs(box[4]) - abs(box[2])) / 2 * sin(2 * pi * 0:nsides / nsides)
      )
      cropped <- sf::st_sf(sf::st_sfc(sf::st_polygon(list(as.matrix(borders)))), crs = 4326)
      newbox <- lapply(sf::st_geometry(cropped), sf::st_bbox)[[1]]
      box <- c(newbox$xmin, newbox$ymin, newbox$xmax, newbox$ymax)
    }
    shiny::incProgress(1 / 13, detail = "[1/13] Downloading water bodies")
    water <- osmdata::osmdata_sf(q = osmdata::opq(bbox = box) |> osmdata::add_osm_feature(key = "natural", value = c("water", "strait", "coastline", "beach", "peninsula")))
    water_multi <- water$osm_multipolygons$geometry
    if (!is.null(water_multi)) {
      water_multi <- sf::st_make_valid(water_multi)
      if (input$border != "None") {
        sf::st_crs(water_multi) <- sf::st_crs(cropped)
        water_multi <- water_multi |> sf::st_intersection(cropped)
      }
    }
    water_poly <- water$osm_polygons$geometry
    if (!is.null(water_poly)) {
      water_poly <- sf::st_make_valid(water_poly)
      if (input$border != "None") {
        sf::st_crs(water_poly) <- sf::st_crs(cropped)
        water_poly <- water_poly |> sf::st_intersection(cropped)
      }
    }
    shiny::incProgress(1 / 13, detail = "[2/13] Downloading waterways")
    waterway <- osmdata::osmdata_sf(q = osmdata::opq(bbox = box) |> osmdata::add_osm_feature(key = "waterway", value = c("river", "riverbank", "stream", "tidal_channel", "canal", "pressurised", "drain", "ditch", "dock", "dam", "weir", "waterfall", "rapids", "lock_gate", "sluice_gate", "bridge")))$osm_polygons$geometry
    if (!is.null(waterway)) {
      waterway <- sf::st_make_valid(waterway)
      if (input$border != "None") {
        sf::st_crs(waterway) <- sf::st_crs(cropped)
        waterway <- waterway |> sf::st_intersection(cropped)
      }
    }
    shiny::incProgress(1 / 13, detail = "[3/13] Downloading coastlines")
    coastlines <- osmdata::osmdata_sf(q = osmdata::opq(bbox = box) |> osmdata::add_osm_feature(key = "natural", value = c("coastline", "beach", "peninsula")))$osm_lines$geometry
    if (!is.null(coastlines)) {
      coastlines <- sf::st_make_valid(coastlines)
      if (input$border != "None") {
        sf::st_crs(coastlines) <- sf::st_crs(cropped)
        coastlines <- coastlines |> sf::st_intersection(cropped)
      }
    }
    shiny::incProgress(1 / 13, detail = "[4/13] Downloading train tracks")
    rails <- osmdata::osmdata_sf(q = osmdata::opq(bbox = box) |> osmdata::add_osm_feature(key = "railway", value = "rail"))$osm_lines$geometry
    if (!is.null(rails)) {
      rails <- sf::st_make_valid(rails)
      if (input$border != "None") {
        sf::st_crs(rails) <- sf::st_crs(cropped)
        rails <- rails |> sf::st_intersection(cropped)
      }
    }
    shiny::incProgress(1 / 13, detail = "[5/13] Downloading runways")
    runway <- osmdata::osmdata_sf(q = osmdata::opq(bbox = box) |> osmdata::add_osm_feature(key = "aeroway", value = "runway"))$osm_lines$geometry
    if (!is.null(runway)) {
      runway <- sf::st_make_valid(runway)
      if (input$border != "None") {
        sf::st_crs(runway) <- sf::st_crs(cropped)
        runway <- runway |> sf::st_intersection(cropped)
      }
    }
    shiny::incProgress(1 / 13, detail = "[6/13] Downloading taxiways")
    taxiway <- osmdata::osmdata_sf(q = osmdata::opq(bbox = box) |> osmdata::add_osm_feature(key = "aeroway", value = "taxiway"))$osm_lines$geometry
    if (!is.null(taxiway)) {
      taxiway <- sf::st_make_valid(taxiway)
      if (input$border != "None") {
        sf::st_crs(taxiway) <- sf::st_crs(cropped)
        taxiway <- taxiway |> sf::st_intersection(cropped)
      }
    }
    shiny::incProgress(1 / 13, detail = "[7/13] Downloading large streets")
    lstreets <- osmdata::osmdata_sf(q = osmdata::opq(bbox = box) |> osmdata::add_osm_feature(key = "highway", value = c("motorway", "primary", "motorway_link", "primary_link")))$osm_lines$geometry
    if (!is.null(lstreets)) {
      lstreets <- sf::st_make_valid(lstreets)
      if (input$border != "None") {
        sf::st_crs(lstreets) <- sf::st_crs(cropped)
        lstreets <- lstreets |> sf::st_intersection(cropped)
      }
    }
    shiny::incProgress(1 / 13, detail = "[8/13] Downloading medium streets")
    mstreets <- osmdata::osmdata_sf(q = osmdata::opq(bbox = box) |> osmdata::add_osm_feature(key = "highway", value = c("secondary", "tertiary", "secondary_link", "tertiary_link")))$osm_lines$geometry
    if (!is.null(mstreets)) {
      mstreets <- sf::st_make_valid(mstreets)
      if (input$border != "None") {
        sf::st_crs(mstreets) <- sf::st_crs(cropped)
        mstreets <- mstreets |> sf::st_intersection(cropped)
      }
    }
    shiny::incProgress(1 / 13, detail = "[9/13] Downloading small streets")
    sstreets <- osmdata::osmdata_sf(q = osmdata::opq(bbox = box) |> osmdata::add_osm_feature(key = "highway", value = c("residential", "living_street", "unclassified", "service", "footway", "cycleway")))$osm_lines$geometry
    if (!is.null(sstreets)) {
      sstreets <- sf::st_make_valid(sstreets)
      if (input$border != "None") {
        sf::st_crs(sstreets) <- sf::st_crs(cropped)
        sstreets <- sstreets |> sf::st_intersection(cropped)
      }
    }
    shiny::incProgress(1 / 13, detail = "[10/13] Downloading trunk streets")
    tstreets <- osmdata::osmdata_sf(q = osmdata::opq(bbox = box) |> osmdata::add_osm_feature(key = "highway", value = c("trunk", "trunk_link", "trunk_loop")))$osm_lines$geometry
    if (!is.null(tstreets)) {
      tstreets <- sf::st_make_valid(tstreets)
      if (input$border != "None") {
        sf::st_crs(tstreets) <- sf::st_crs(cropped)
        tstreets <- tstreets |> sf::st_intersection(cropped)
      }
    }
    shiny::incProgress(1 / 13, detail = "[11/13] Downloading buildings")
    buildings <- osmdata::osmdata_sf(q = osmdata::opq(bbox = box) |> osmdata::add_osm_feature(key = "building"))$osm_polygons$geometry
    if (!is.null(buildings)) {
      buildings <- sf::st_make_valid(buildings)
      if (input$border != "None") {
        sf::st_crs(buildings) <- sf::st_crs(cropped)
        buildings <- buildings |> sf::st_intersection(cropped)
      }
    }
    incProgress(1 / 13, detail = "[12/13] Putting it all together")
    int_p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = water_poly, fill = input$waterColor, color = input$lineColor, size = 0.1, inherit.aes = FALSE) +
      ggplot2::geom_sf(data = water_multi, fill = input$waterColor, color = input$lineColor, size = 0.1, inherit.aes = FALSE) +
      ggplot2::geom_sf(data = waterway, fill = input$waterColor, color = input$lineColor, size = 0.1, inherit.aes = FALSE) +
      ggplot2::geom_sf(data = coastlines, color = input$lineColor, size = 0.125, inherit.aes = FALSE) +
      ggplot2::geom_sf(data = rails, color = input$lineColor, size = 0.2, inherit.aes = FALSE) +
      ggplot2::geom_sf(data = runway, color = input$lineColor, size = 3, inherit.aes = FALSE, alpha = 0.2) +
      ggplot2::geom_sf(data = taxiway, color = input$lineColor, size = 0.7, inherit.aes = FALSE, alpha = 0.2) +
      ggplot2::geom_sf(data = mstreets, color = input$lineColor, size = 0.2, inherit.aes = FALSE, alpha = 0.6) +
      ggplot2::geom_sf(data = sstreets, color = input$lineColor, size = 0.125, inherit.aes = FALSE, alpha = 0.7) +
      ggplot2::geom_sf(data = lstreets, color = input$lineColor, size = 0.3, inherit.aes = FALSE, alpha = 0.6) +
      ggplot2::geom_sf(data = tstreets, color = input$lineColor, size = 0.3, inherit.aes = FALSE, alpha = 0.6) +
      ggplot2::geom_sf(data = buildings, fill = input$mapColor, color = input$lineColor, size = 0.1, inherit.aes = FALSE) +
      ggplot2::coord_sf(xlim = c(box[1], box[3]), ylim = c(box[2], box[4]), expand = TRUE) +
      ggplot2::theme_void() +
      ggplot2::theme(plot.margin = ggplot2::margin(4, 0, 0, 0, "cm"))
    if (input$border != "None") {
      suppressMessages(expr = {
        int_p <- int_p + ggplot2::geom_sf(data = cropped, fill = NA, color = input$mapColor, size = 1) +
          ggplot2::geom_path(data = borders, mapping = ggplot2::aes(x = x, y = y), color = input$lineColor, size = 0.2, inherit.aes = FALSE)
      })
    }
    p <- cowplot::ggdraw(int_p) +
      cowplot::draw_text(text = input$plotTitle, x = 0.5, y = 0.93, size = 110, color = input$lineColor, family = "Caveat", fontface = "bold") +
      cowplot::draw_text(text = input$countryTitle, x = 0.5, y = 0.975, size = 50, color = input$lineColor, family = "Caveat") +
      ggspatial::annotation_north_arrow(
        location = "bl", height = ggplot2::unit(3, "cm"), width = ggplot2::unit(3, "cm"),
        pad_x = ggplot2::unit(1, "cm"), pad_y = ggplot2::unit(1, "cm"),
        style = ggspatial::north_arrow_nautical(line_col = input$lineColor, text_size = 20, text_face = "bold", text_family = "Caveat", text_col = input$lineColor, fill = c(input$lineColor, input$mapColor))
      ) +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = input$mapColor, color = input$mapColor),
        panel.background = ggplot2::element_rect(fill = input$mapColor, color = input$mapColor)
      )
    if (latitude < 0) {
      lat <- paste0(format(abs(latitude), digits = 6), "\u00B0 S")
    } else {
      lat <- paste0(format(latitude, digits = 6), "\u00B0 N")
    }
    if (longitude < 0) {
      long <- paste0(format(abs(longitude), digits = 6), "\u00B0 W")
    } else {
      long <- paste0(format(longitude, digits = 6), "\u00B0 E")
    }
    p <- p + cowplot::draw_text(text = paste0(lat, " / ", long), x = 0.97, y = 0.03, size = 40, color = input$lineColor, family = "Caveat", hjust = 1)
    incProgress(1 / 13, detail = "[13/13] Rendering plot")
  })
  return(p)
}

circle_border <- function(long, lat, rlong, rlat, npoints = 1000) {
  tt <- seq(0, 2 * pi, length.out = npoints)
  xx <- long + rlong * cos(tt)
  yy <- lat + rlat * sin(tt)
  return(data.frame(x = xx, y = yy))
}

get_city <- function(seed) {
  set.seed(seed)
  cities <- maps::world.cities
  cities <- subset(cities, cities$pop > 100000)
  index <- sample(1:nrow(cities), size = 1)
  city <- cities[index, ]
  return(city)
}

# 3. UI
ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("flatly"),
  tags$head(HTML("<title>R City Views</title>")),
  shiny::fluidRow(align = "center", titlePanel(HTML("<h1><b>R City Views</b></h1>"))),
  hr(),
  fluidRow(
    column(3,
      style = "border: 4px double black;",
      shiny::fluidRow(align = "center", HTML("<h2>Instructions</h2>")),
      HTML("<p><b>Step 1:</b> Drag the map, click <span style='color: #edb92e'><b>Randomize</b></span>, or use the search tool to find the area that you want to render.</p>
                     <p><b>Step 2:</b> Adjust the name of the city under <b>City name</b> and the country under <b>Country</b>.</p>
                     <p><b>Step 3:</b> Click the <span style='color: #f85931'><b>Preview</b></span> button to preview the map in the panel below (this may take a while).</p>
                     <p><b>Step 4:</b> Once the image is displayed you can click the <span style='color: #009989'><b>Download</b></span> button to export it as an <b>.svg</b> file.</p>
                     <p><b>Note:</b> Previewing a (too) large or populated area may disconnect you from the server due to a data limit of 1 GB for free Shiny subscriptions. In this case, download <a href='https://raw.githubusercontent.com/koenderks/rcityviews/master/app.R' target='_blank'>app.R</a> from GitHub and run on your own computer.</p>"),
    ),
    column(4,
      offset = 1,
      shiny::fluidRow(align = "center", leafletOutput(outputId = "osm", width = "500px", height = "410px"))
    ),
    column(3,
      offset = 1,
      shiny::fluidRow(align = "center", shiny::textInput(inputId = "plotTitle", label = "City name", value = "")),
      shiny::fluidRow(align = "center", shiny::textInput(inputId = "countryTitle", label = "Country", value = "")),
      shiny::fluidRow(align = "center", shiny::selectInput(inputId = "border", label = "Border style", choices = c("None", "Circle", "Rhombus", "Square", "Hexagon", "Octagon", "Decagon"))),
      shiny::fluidRow(align = "center", shiny::checkboxInput(inputId = "show", label = "Show color options")),
      shiny::fluidRow(align = "center", colourpicker::colourInput(inputId = "lineColor", label = "Streets / Rails", value = "#32130f")),
      shiny::fluidRow(align = "center", colourpicker::colourInput(inputId = "mapColor", label = "Background", value = "#fdf9f5")),
      shiny::fluidRow(align = "center", colourpicker::colourInput(inputId = "waterColor", label = "Water", value = "#fdf9f5")),
      shiny::fluidRow(
        align = "center",
        shiny::actionButton(inputId = "randomize", label = "Randomize", icon = icon("dice"), style = "color: #000000; background-color: #edb92e; border-color: #000000"),
        shiny::actionButton(inputId = "run", label = "Preview", icon = icon("camera"), style = "color: #000000; background-color: #f85931; border-color: #000000"),
        shiny::downloadButton(outputId = "downloadPlot", label = "Download", style = "color: #000000; background-color: #009989; border-color: #000000")
      ),
      HTML("<br>"),
      shiny::fluidRow(
        align = "center",
        tags$a(href = "https://koenderks.shinyapps.io/rcityviews/", "Tweet", class = "twitter-share-button"),
        includeScript("http://platform.twitter.com/widgets.js")
      )
    )
  ),
  hr(),
  mainPanel(
    width = 12, style = "border: 4px double black;",
    shiny::fluidRow(align = "center", plotOutput(outputId = "plotObject", width = "1400px", height = "1600px"))
  )
)

# 4. Server
server <- function(input, output, session) {
  city <- get_city(sample(1:100000, size = 1))
  updateTextInput(session, "plotTitle", value = city$name)
  updateTextInput(session, "countryTitle", value = city$country)
  output$osm <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng = city$long, lat = city$lat, zoom = 14) |>
      leaflet.extras::addSearchOSM()
  })
  observe({
    toggle("lineColor", anim = FALSE, condition = input$show)
    toggle("mapColor", anim = FALSE, condition = input$show)
    toggle("waterColor", anim = FALSE, condition = input$show)
  })
  observeEvent(input$randomize, {
    city <- get_city(sample(1:100000, size = 1))
    updateTextInput(session, "plotTitle", value = city$name)
    updateTextInput(session, "countryTitle", value = city$country)
    output$osm <- renderLeaflet({
      leaflet() |>
        addTiles() |>
        setView(lng = city$long, lat = city$lat, zoom = 14) |>
        leaflet.extras::addSearchOSM()
    })
  })
  output$plotObject <- renderPlot(NULL)
  observeEvent(input$run, {
    p <- draw_plot(input)
    output$plotObject <- shiny::renderPlot(p)
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste0(input$plotTitle, ".svg")
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = p, height = 500, width = 500, units = "mm")
      }
    )
  })
}

# 5. Run app
shinyApp(ui = ui, server = server)
