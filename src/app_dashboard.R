#####################################################################
### Reads in census data + maps
### Generates Shiny dashboard with widgets for charging levels
### Produces scatter plot with clickable points that show tables/maps
#####################################################################

library(dplyr)
library(ggplot2)
library(here)
library(htmltools)
library(leaflet)
library(magrittr)
library(sf)
library(shiny)
library(tidyverse)


# --- Config ---
select_city <- "Tor"
data_dir <- here("data")

# Read ZEV stock data
zev_file <- here(data_dir, paste0("ZEV_stock_for_", select_city, ".geojson"))
ZEV_stock <- st_read(zev_file, quiet = TRUE) %>%
  mutate(Population = as.numeric(Population))

# CT geometry for 2022
CTUID_geo_map <- ZEV_stock %>%
  filter(year == 2022) %>%
  select(CTUID, geometry) %>%
  st_sf()

# --- Load curves ---
colnameVec <- sprintf("%02d", 0:23)
load_curve_S <- data.frame(matrix(
  c(0.7,0.6,0.6,0.6,0.6,0.6,0.7,0.8,0.8,0.8,0.7,0.7,
    0.7,0.7,0.7,0.7,0.85,0.85,0.85,0.95,0.9,1.0,0.9,0.8),
  nrow = 1, byrow = TRUE), check.names = FALSE)
load_curve_W <- data.frame(matrix(
  c(0.7,0.6,0.6,0.6,0.6,0.6,0.7,0.8,0.8,0.8,0.7,0.7,
    0.7,0.7,0.7,0.7,0.8,0.8,0.8,0.9,0.9,1.0,0.9,0.8),
  nrow = 1, byrow = TRUE), check.names = FALSE)
colnames(load_curve_S) <- colnames(load_curve_W) <- colnameVec

# Scale curves by peak daily load per person (2.125 kW)
load_curve_S <- load_curve_S * 2.125
load_curve_W <- load_curve_W * 2.125

# Transformer capacity per household
load_capacity <- data.frame(load = ZEV_stock$Population * 2.5)

# --- UI ---
ui <- fluidPage(
  titlePanel("Transformer Peak Loads by Census Tract, 2022-2046"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("capacity_type", "Transformer load measurement",
                   c("Load (KW)" = "level", "Capacity utilization rate (%)" = "cap_u")),
      radioButtons("load_curve", "Load curve",
                   c("Winter Peak" = "winter", "Summer Peak" = "summer")),
      sliderInput("charge_level1", "Share of level 1 charging (%)", min = 0, max = 100, value = 70),
      sliderInput("diversification", "Share of charging during evening peak (%)", min = 0, max = 100, value = 30)
    ),
    mainPanel(
      plotOutput("MyPlot", height = 300, click = "plot_click"),
      tableOutput("PlotClickInfo"),
      leafletOutput(outputId = "MyMap")
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {

  # Reactive values
  charge_level1 <- reactive(1 - 0.01 * input$charge_level1)
  diversification <- reactive(0.01 * input$diversification)
  charge_level <- reactive(charge_level1() * 1.00 + (1 - charge_level1()) * 3.60)

  load_curve <- reactive(switch(input$load_curve, summer = load_curve_S, winter = load_curve_W))

  # Base + charging loads
  base_load <- reactive(ZEV_stock$Population * load_curve()$`19`)
  charge_load <- reactive(ZEV_stock$ZEV_stock * charge_level1() * diversification())

  load_init_level <- reactive(data.frame(load = base_load() + charge_load(),
                                         year = ZEV_stock$year,
                                         CTUID = ZEV_stock$CTUID))
  load_init_cap_u <- reactive(data.frame(load = (base_load() + charge_load()) / load_capacity$load,
                                         year = ZEV_stock$year,
                                         CTUID = ZEV_stock$CTUID))

  load_final <- reactive(switch(input$capacity_type, level = load_init_level(), cap_u = load_init_cap_u()))
  load_geo_init <- reactive(cbind(load_final(), geometry = ZEV_stock$geometry) %>% st_sf())

  # Labels & bins
  graph_title <- reactive(switch(input$capacity_type,
                                 level = "Transformer Load (KW), 2022-2046",
                                 cap_u = "Transformer Capacity Utilization Rate (%), 2022-2046"))
  load_label <- reactive(switch(input$capacity_type,
                                level = "Load (KW):",
                                cap_u = "Capacity Utilization Rate (percent):"))
  bins <- reactive(switch(input$capacity_type,
                          level = c(0,2000,4000,6000,8000,10000,12000,14000,Inf),
                          cap_u = c(0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,Inf)))

  # --- PLOTS ---
  output$MyPlot <- renderPlot({
    ggplot(load_final(), aes(x = year, y = load)) +
      geom_point() +
      ggtitle(graph_title()) +
      xlab("Year") + ylab(load_label()) +
      theme(legend.position = "none")
  })

  # Table of clicked points
  output$PlotClickInfo <- renderTable({ nearPoints(load_final(), input$plot_click) })

  # Subset geographic data to clicked points
  load_geo <- reactive({
    semi_join(load_geo_init(), nearPoints(load_final(), input$plot_click), by = "CTUID") %>% st_sf()
  })

  # --- LEAFLET MAP ---
  output$MyMap <- renderLeaflet({
    palCT <- colorBin("Reds", domain = load_geo()$load, bins = bins())
    labelsCT <- sprintf("%s %0.2f<br/>CTUID: %s", load_label(), load_geo()$load, load_geo()$CTUID) %>%
      lapply(HTML)

    leaflet(load_geo()) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = CTUID_geo_map, color = "grey", weight = 1, opacity = 1, fillOpacity = 0.65) %>%
      {if (!is.null(input$plot_click)) addPolygons(., fillColor = ~palCT(load),
                                                   color = "white", weight = 1, opacity = 1, fillOpacity = 0.65,
                                                   highlightOptions = highlightOptions(color = "grey", weight = 3, bringToFront = TRUE),
                                                   label = labelsCT,
                                                   labelOptions = labelOptions(textsize = "15px")) %>%
          addLegend(pal = palCT, values = load_geo()$load, opacity = 0.9,
                    title = load_label(), position = "bottomleft") else .}
  })
}

shinyApp(ui = ui, server = server)
