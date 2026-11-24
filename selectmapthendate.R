library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(lubridate)
library(here)

# ---- Data ------------------------------------------------------------------
male <- read_csv(here("data/elephant_fixes_2025male.csv"), show_col_types = FALSE) |>
  mutate(elephant = recode(elephant, "E01" = "m1"))
female <- read_csv(here("data/elephant_fixes_2025female.csv"), show_col_types = FALSE) |>
  mutate(elephant = recode(elephant, "E01" = "f1"))

elephants <- bind_rows(male, female) |>
  select(elephant, datetime, lon, lat) |>
  mutate(date = as_date(datetime)) |>
  arrange(elephant, datetime)

basemaps <- c(
  "OpenStreetMap"      = "OpenStreetMap",
  "Light (CartoDB)"    = "CartoDB.Positron",
  "Satellite (Esri)"   = "Esri.WorldImagery",
  "Topographic"        = "OpenTopoMap"
)

# ---- UI --------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Elephant tracks"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("elephant", "Elephant", choices = sort(unique(elephants$elephant))),
      selectInput("basemap", "Map", choices = basemaps),
      selectInput("day", "Day", choices = NULL),
      helpText("Days are listed with their number of GPS fixes."),
      hr(),
      tableOutput("summary")
    ),
    mainPanel(
      width = 9,
      leafletOutput("map", height = "650px")
    )
  )
)

# ---- Server ----------------------------------------------------------------
server <- function(input, output, session) {
  
  ele_data <- reactive({
    req(input$elephant)
    filter(elephants, elephant == input$elephant)
  })
  
  # Identify the days available for the selected elephant
  days <- reactive({
    ele_data() |>
      count(date, name = "fixes")
  })
  
  observeEvent(days(), {
    d <- days()
    choices <- c(
      "All days" = "all",
      setNames(as.character(d$date), paste0(d$date, " (", d$fixes, " fixes)"))
    )
    updateSelectInput(session, "day", choices = choices, selected = "all")
  })
  
  selected <- reactive({
    req(input$day)
    d <- ele_data()
    if (input$day == "all") d else filter(d, date == as.Date(input$day))
  })
  
  output$summary <- renderTable({
    ele_data() |>
      summarise(
        `First day` = as.character(min(date)),
        `Last day`  = as.character(max(date)),
        Days        = n_distinct(date),
        Fixes       = n()
      ) |>
      tidyr::pivot_longer(everything(), names_to = "", values_to = "value") |>
      mutate(value = as.character(value))
  })
  
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(isolate(input$basemap))
  })
  
  # Swap the basemap without redrawing the tracks
  observeEvent(input$basemap, {
    leafletProxy("map") |>
      clearTiles() |>
      addProviderTiles(input$basemap)
  }, ignoreInit = TRUE)
  
  # Draw the selected track(s)
  observe({
    d <- selected()
    req(nrow(d) > 0)
    
    pal <- colorFactor("viridis", domain = as.character(d$date))
    
    proxy <- leafletProxy("map") |>
      clearShapes() |>
      clearMarkers()
    
    for (dt in unique(as.character(d$date))) {
      seg <- filter(d, as.character(date) == dt)
      proxy <- proxy |>
        addPolylines(data = seg, lng = ~lon, lat = ~lat,
                     color = pal(dt), weight = 3, opacity = 0.8) |>
        addCircleMarkers(data = seg, lng = ~lon, lat = ~lat,
                         radius = 3, color = pal(dt), stroke = FALSE,
                         fillOpacity = 0.8,
                         label = ~format(datetime, "%Y-%m-%d %H:%M"))
    }
    
    proxy |>
      fitBounds(min(d$lon), min(d$lat), max(d$lon), max(d$lat))
  })
}

shinyApp(ui, server)

