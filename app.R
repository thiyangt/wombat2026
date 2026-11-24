library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(lubridate)

# ---- 1. Load data -----------------------------------------------------------
# Reads data/elephant_fixes_2025male.csv (relative to this file), with columns
# elephant, datetime, lon, lat. If it is not found, a simulated demo track is
# used so the app still runs.

load_data <- function() {
  if (file.exists("data/elephant_fixes_2025male.csv")) {
    d <- readr::read_csv("data/elephant_fixes_2025male.csv", show_col_types = FALSE)
    d <- d[, c("elephant", "datetime", "lon", "lat")]
    d$datetime <- as.POSIXct(d$datetime, tz = "UTC")
  } else {
    set.seed(1)
    n <- 8760
    heading <- cumsum(rnorm(n, 0, 0.4))
    hour <- rep(0:23, length.out = n)
    speed <- ifelse(hour %in% 10:15, 0.02,            # midday rest
                    ifelse(hour %in% c(6:9, 16:19), 0.15, 0.05)) * runif(n, 0.3, 1.5)
    d <- tibble(
      elephant = "E01",
      datetime = seq(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
                     by = "hour", length.out = n),
      lon = 80.8 + cumsum(speed * cos(heading)) * 0.05,
      lat = 6.51 + cumsum(speed * sin(heading)) * 0.05
    )
  }
  d
}

haversine_km <- function(lon1, lat1, lon2, lat2) {
  r <- 6371
  to_rad <- pi / 180
  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  a <- sin(dlat / 2)^2 +
    cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  2 * r * asin(pmin(1, sqrt(a)))
}

dat <- load_data() |>
  arrange(elephant, datetime) |>
  group_by(elephant) |>
  mutate(
    dt_h      = as.numeric(difftime(datetime, lag(datetime), units = "hours")),
    step_km   = haversine_km(lag(lon), lag(lat), lon, lat),
    speed_kmh = step_km / dt_h,
    date      = as.Date(datetime, tz = "UTC"),
    hour      = hour(datetime)
  ) |>
  ungroup()

d_min <- min(dat$date)
d_max <- max(dat$date)
behaviours <- c("Resting", "Foraging", "Travelling")
pal <- colorFactor(c("#4575b4", "#fdae61", "#d73027"),
                   domain = behaviours, ordered = TRUE)

# ---- 2. UI ------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Elephant movement and behaviour"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("elephant", "Elephant", choices = sort(unique(dat$elephant))),
      
      dateInput("date", "Select date", value = d_min,
                min = d_min, max = d_max, weekstart = 1),
      
      sliderInput("hour", "Select time (hour, 0-23)",
                  min = 0, max = 23, value = 0, step = 1,
                  animate = animationOptions(interval = 500, loop = FALSE)),
      
      sliderInput("trail", "Trail length (hours)", min = 1, max = 24,
                  value = 6, step = 1),
      
      tags$hr(),
      strong("Behaviour thresholds (speed, km/h)"),
      sliderInput("thr", NULL, min = 0, max = 1.5, value = c(0.05, 0.3),
                  step = 0.01),
      helpText("Below the lower value = Resting; between = Foraging;",
               "above the upper value = Travelling."),
      tags$hr(),
      uiOutput("current_info")
    ),
    mainPanel(
      width = 9,
      leafletOutput("map", height = 520),
      plotOutput("speed_plot", height = 220)
    )
  )
)

# ---- 3. Server --------------------------------------------------------------
server <- function(input, output, session) {
  
  # data for one elephant, with behaviour classes
  ele <- reactive({
    dat |>
      filter(elephant == input$elephant) |>
      mutate(behaviour = factor(
        case_when(
          is.na(speed_kmh)         ~ NA_character_,
          speed_kmh < input$thr[1] ~ "Resting",
          speed_kmh < input$thr[2] ~ "Foraging",
          TRUE                     ~ "Travelling"
        ), levels = behaviours))
  })
  
  # the selected day
  day <- reactive({
    d <- ele() |> filter(date == input$date)
    validate(need(nrow(d) > 0, "No data for this elephant on the selected date."))
    d
  })
  
  # the fix at the selected hour
  current <- reactive({
    cur <- day() |> filter(hour == input$hour)
    validate(need(nrow(cur) > 0, "No fix at this hour."))
    cur[1, ]
  })
  
  # trail leading up to the selected time (may reach into the previous day)
  trail <- reactive({
    now <- current()$datetime
    ele() |> filter(datetime <= now, datetime >= now - input$trail * 3600)
  })
  
  # base map
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
      addProviderTiles(providers$OpenStreetMap, group = "Streets") |>
      addLayersControl(baseGroups = c("Satellite", "Streets"),
                       options = layersControlOptions(collapsed = TRUE)) |>
      addLegend("bottomright", pal = pal, values = behaviours,
                title = "Behaviour", opacity = 1)
  })
  
  # fit map to the selected day when date or elephant changes
  observeEvent(list(input$date, input$elephant), {
    d <- ele() |> filter(date == input$date)
    req(nrow(d) > 0)
    leafletProxy("map") |>
      fitBounds(min(d$lon) - 0.002, min(d$lat) - 0.002,
                max(d$lon) + 0.002, max(d$lat) + 0.002)
  })
  
  # redraw tracks whenever the time changes
  observe({
    d <- day(); tr <- trail(); cur <- current()
    leafletProxy("map") |>
      clearShapes() |> clearMarkers() |>
      addPolylines(data = d, lng = ~lon, lat = ~lat,
                   color = "white", weight = 1.5, opacity = 0.5) |>
      addPolylines(data = tr, lng = ~lon, lat = ~lat,
                   color = "yellow", weight = 3, opacity = 0.9) |>
      addCircleMarkers(data = tr, lng = ~lon, lat = ~lat, radius = 4,
                       color = ~pal(behaviour), fillOpacity = 0.9, stroke = FALSE) |>
      addCircleMarkers(data = cur, lng = ~lon, lat = ~lat, radius = 9,
                       color = "black", weight = 2,
                       fillColor = ~pal(behaviour), fillOpacity = 1,
                       label = ~format(datetime, "%d %b %Y %H:%M"))
  })
  
  # speed through the day with a marker for the selected hour
  output$speed_plot <- renderPlot({
    d <- day()
    ggplot(d, aes(hour, speed_kmh)) +
      geom_col(aes(fill = behaviour), width = 0.9, na.rm = TRUE) +
      geom_vline(xintercept = input$hour, colour = "black", linewidth = 0.8) +
      scale_x_continuous(breaks = seq(0, 23, 3), limits = c(-0.6, 23.6)) +
      scale_fill_manual(values = c(Resting = "#4575b4", Foraging = "#fdae61",
                                   Travelling = "#d73027"), drop = FALSE) +
      labs(x = "Hour of day", y = "Speed (km/h)", fill = "Behaviour") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })
  
  # summary panel
  output$current_info <- renderUI({
    cur <- current(); d <- day()
    tagList(
      strong("Selected time"), p(format(cur$datetime, "%d %b %Y %H:%M")),
      strong("Behaviour"),     p(as.character(cur$behaviour)),
      strong("Speed"),         p(sprintf("%.2f km/h", cur$speed_kmh)),
      strong("Distance this day"), p(sprintf("%.1f km", sum(d$step_km, na.rm = TRUE))),
      strong("Day summary"),
      p(sprintf("%.0f%% resting, %.0f%% foraging, %.0f%% travelling",
                100 * mean(d$behaviour == "Resting", na.rm = TRUE),
                100 * mean(d$behaviour == "Foraging", na.rm = TRUE),
                100 * mean(d$behaviour == "Travelling", na.rm = TRUE)))
    )
  })
}

shinyApp(ui, server)