#####################################################################
### Sidebar + Table + Map, no pipes to allow control mapping
#####################################################################
### Reads in census data + maps
### Generates Shiny dashboard with widgets for charging levels, etc.
### and produces scatter plot with clickable data points (census
### tract loads) that, in turn, produces table and map of clicked data points
#####################################################################

library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)

# === CONFIGURATION ===
# Cities: "Tor", "Ott", "Ham", "KCW", "Lon", "SCN", "Win", "Osh", "Van", "Vic"
select_city <- "Tor"
ZEV_file_name <- paste0("ZEV_stock_for_", select_city, ".geojson")

# === DATA LOAD ===
ZEV_stock <- st_read(ZEV_file_name, quiet = TRUE) %>%
  mutate(Population = as.numeric(Population))

CTUID_geo_map <- ZEV_stock %>%
  filter(year == "2022") %>%
  select(CTUID, geometry)

load_curves <- list(
  summer = c(0.7, 0.6, 0.6, 0.6, 0.6, 0.6, 0.7, 0.8, 0.8, 0.8, 0.7, 0.7,
             0.7, 0.7, 0.7, 0.7, 0.85, 0.85, 0.85, 0.95, 0.9, 1.0, 0.9, 0.8),
  winter = c(0.7, 0.6, 0.6, 0.6, 0.6, 0.6, 0.7, 0.8, 0.8, 0.8, 0.7, 0.7,
             0.7, 0.7, 0.7, 0.7, 0.8, 0.8, 0.8, 0.9, 0.9, 1.0, 0.9, 0.8)
) %>%
  lapply(function(x) setNames(as.data.frame(t(x) * 2.125), sprintf("%02d", 0:23)))

load_capacity <- tibble(load = ZEV_stock$Population * 2.5)

# === UI ===
ui <- fluidPage(
  titlePanel("Transformer Peak Loads by Census Tract, 2022–2046"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("capacity_type", "Transformer load measurement",
                   c("Load (kW)" = "level", "Capacity utilization rate (%)" = "cap_u")),
      radioButtons("load_curve", "Load curve",
                   c("Winter Peak" = "winter", "Summer Peak" = "summer")),
      sliderInput("charge_level1", "Share of level 1 charging (%)", min = 0, max = 100, value = 70),
      sliderInput("diversification", "Share of charging during evening peak (%)", min = 0, max = 100, value = 30)
    ),
    mainPanel(
      plotOutput("MyPlot", height = 300, click = "plot_click"),
      tableOutput("PlotClickInfo"),
      leafletOutput("MyMap")
    )
  )
)

# === SERVER ===
server <- function(input, output, session) {
  rv <- reactiveValues(
    clicked_data = NULL
  )

  load_curve <- reactive({
    load_curves[[input$load_curve]]
  })

  base_load <- reactive({
    ZEV_stock$Population * load_curve()[["19"]]
  })

  charge_load <- reactive({
    share_lvl1 <- 1 - 0.01 * input$charge_level1
    diversification <- 0.01 * input$diversification
    charge_level <- share_lvl1 * 1.00 + (1 - share_lvl1) * 3.60
    ZEV_stock$ZEV_stock * share_lvl1 * diversification
  })

  load_data <- reactive({
    df <- tibble(
      CTUID = ZEV_stock$CTUID,
      year = ZEV_stock$year,
      load = base_load() + charge_load()
    )
    if (input$capacity_type == "cap_u") {
      df$load <- df$load / load_capacity$load
    }
    df
  })

  load_label <- reactive({
    if (input$capacity_type == "level") "Load (kW):" else "Capacity Utilization Rate (%):"
  })

  graph_title <- reactive({
    if (input$capacity_type == "level") {
      "Transformer Load (kW), 2022–2046"
    } else {
      "Transformer Capacity Utilization Rate (%), 2022–2046"
    }
  })

  bins <- reactive({
    if (input$capacity_type == "level") {
      c(0, 2000, 4000, 6000, 8000, 10000, 12000, 14000, Inf)
    } else {
      c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, Inf)
    }
  })

  # Track user click and store the filtered data
  observeEvent(input$plot_click, {
    near <- nearPoints(load_data(), input$plot_click, threshold = 10)
    if (nrow(near) > 0) {
      geo <- left_join(near, CTUID_geo_map, by = "CTUID") %>%
        st_as_sf()
      rv$clicked_data <- geo
    }
  })

  # === PLOT ===
  output$MyPlot <- renderPlot({
    ggplot(load_data(), aes(x = year, y = load)) +
      geom_point() +
      labs(title = graph_title(), x = "Year", y = load_label()) +
      theme_minimal()
  })

  # === TABLE ===
  output$PlotClickInfo <- renderTable({
    if (!is.null(rv$clicked_data)) {
      st_drop_geometry(rv$clicked_data)
    }
  })

  # === MAP ===
  output$MyMap <- renderLeaflet({
    # Base geometry layer (gray background)
    base_map <- leaflet(CTUID_geo_map) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(color = "grey", weight = 1, fillOpacity = 0.2)

    # Only show color overlay if clicked
    if (!is.null(rv$clicked_data)) {
      geo <- rv$clicked_data
      pal <- colorBin("Reds", domain = geo$load, bins = bins())
      labels <- sprintf("%s %.2f<br/>CTUID: %s", load_label(), geo$load, geo$CTUID) %>%
        lapply(htmltools::HTML)

      base_map <- base_map %>%
        addPolygons(data = geo,
                    fillColor = ~pal(load),
                    color = "white", weight = 2, fillOpacity = 0.8,
                    label = labels,
                    highlightOptions = highlightOptions(weight = 3, color = "black", bringToFront = TRUE)) %>%
        addLegend(pal = pal, values = geo$load, opacity = 0.9,
                  title = load_label(), position = "bottomleft")
    }

    base_map
  })

}

# === RUN APP ===
shinyApp(ui, server)
