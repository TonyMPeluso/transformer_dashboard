#####################################################################
### Reads in census data + maps
### Generates Shiny dashboard with widgets for charging levels, etc
### and produces scatter plot with clickable data points (census
### tract loads) that,in turn produce table and map of clicked data
### points
#####################################################################

library(dplyr)
library(ggplot2)
library(htmltools)
library(leaflet)
library(magrittr)
library(sf)
library(shiny)
library(tidyverse) # Keeping for potential implicit use
# runApp('src') # If your app_02_Dashboar.R is in the 'src' directory


# Define the selected city and construct the file path
# Cities: "Tor", "Ott", "Ham", "KCW", "Lon", "SCN", "Win", "Osh", "Van", "Vic"
select_city <- "Tor"

ZEV_file_name <- paste('../data/ZEV_stock_for_', select_city, '.geojson', sep = "")

# Read the ZEV stock data from the GeoJSON fileg
ZEV_stock <- st_read(ZEV_file_name)
ZEV_stock$Population <- as.numeric(ZEV_stock$Population)

# Produce geographic layer of CTs for the year 2022
CTUID_geo_map <- filter(ZEV_stock, year == "2022") %>%
  subset(., select = c("CTUID", "geometry")) %>%
  st_sf()

# Define load curve distributions for summer and winter peaks
load_curve_S <- as.data.frame(matrix(data = c(
  0.7, 0.6, 0.6, 0.6, 0.6, 0.6, 0.7, 0.8, 0.8, 0.8, 0.7, 0.7,
  0.7, 0.7, 0.7, 0.7, 0.85, 0.85, 0.85, 0.95, 0.9, 1.0, 0.9, 0.8), nrow=1))
load_curve_W <- as.data.frame(matrix(data = c(
  0.7, 0.6, 0.6, 0.6, 0.6, 0.6, 0.7, 0.8, 0.8, 0.8, 0.7, 0.7,
  0.7, 0.7, 0.7, 0.7, 0.8, 0.8, 0.8, 0.9, 0.9, 1.0, 0.9, 0.8), nrow=1))
colnameVec <- c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11",
                "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23")
colnames(load_curve_S) <- colnameVec
colnames(load_curve_W) <- colnameVec

# Multiply load distribution by the peak daily load per person (2.125 kW)
load_curve_S <- load_curve_S * 2.125
load_curve_W <- load_curve_W * 2.125

# Calculate transformer capacity per household
load_capacity <- as.data.frame(ZEV_stock$Population * 2.5) %>%
  setNames(., c("load"))

# User Interface definition
ui <- fluidPage(
  
  titlePanel("Transformer Peak Loads by Census Tract, 2022-2046"),
  
  sidebarLayout(
    
    sidebarPanel(
      radioButtons("capacity_type", "Transformer load measurement",
                   c("Load (KW)" = "level", "Capacity utilization rate (%)" = "cap_u")),
      radioButtons("load_curve", "Load curve",
                   c("Winter Peak" = "winter", "Summer Peak" = "summer")),
      sliderInput("charge_level1", "Share of level 1 charging (%)",
                  min = 0, max = 100, value = 70),
      sliderInput("diversification", "Share of charging during evening peak (%)",
                  min = 0, max = 100, value = 30)
    ),
    
    mainPanel(
      plotOutput("MyPlot", height = 300, click = "plot_click"),
      tableOutput("PlotClickInfo"),
      leafletOutput(outputId = "MyMap")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Reactive expressions for widget inputs
  charge_level1Input <- reactive({as.numeric(1 - 0.01 * input$charge_level1)})
  diversificationInput <- reactive({as.numeric(0.01 * input$diversification)})
  
  # Calculates instantaneous charge based on charging levels
  charge_level <- reactive({charge_level1Input() * 1.00 +
      (1 - charge_level1Input()) * 3.60
  })
  
  # Switches between load curves based on input
  load_curve <- reactive({
    switch(input$load_curve,
           summer = load_curve_S,
           winter = load_curve_W
    )
  })
  
  # Calculates base load and charge load
  base_load <- reactive({data.frame(ZEV_stock$Population * load_curve()$'19')})
  charge_load <- reactive({data.frame(ZEV_stock$ZEV_stock * charge_level1Input() *
                                        diversificationInput())})
  
  # Calculates initial load (level) and capacity utilization rate
  load_init_level <- reactive({data.frame(base_load() + charge_load()) %>%
      cbind(ZEV_stock$year, ZEV_stock$CTUID) %>%
      setNames(., c("load", "year", "CTUID"))
  })
  load_init_cap_u <- reactive({data.frame((base_load() + charge_load()) / load_capacity$load) %>%
      cbind(ZEV_stock$year, ZEV_stock$CTUID) %>%
      setNames(., c("load", "year", "CTUID"))
  })
  
  # Chooses appropriate load measure for table and map
  load_final <- reactive({
    switch(input$capacity_type,
           level = load_init_level(),
           cap_u = load_init_cap_u()
    )
  })
  load_geo_init <- reactive({
    cbind(load_final(), ZEV_stock$geometry) %>%
      set_colnames(., c("load", "year", "CTUID", "geometry"))
  })
  
  # Reactive expression for graph title
  graph_title <- reactive({
    switch(input$capacity_type,
           level = "Transformer Load (KW), 2022-2046",
           cap_u = "Transformer Capacity Utilization Rate (%), 2022-2046"
    )
  })
  
  # Reactive expression for load label
  load_label <- reactive({
    switch(input$capacity_type,
           level = "Load (KW):",
           cap_u = "Capacity Utilization Rate (percent):"
    )
  })
  
  # Reactive expression for color bins
  bins <- reactive({
    switch(input$capacity_type,
           level = c(0, 2000, 4000, 6000, 8000, 10000, 12000, 14000, Inf),
           cap_u = c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, Inf)
    )
  })
  
  # Render the scatter plot
  output$MyPlot <- renderPlot({
    ggplot(load_final(), aes(x=year, y=load)) +
      geom_point() +
      ggtitle(graph_title()) +
      xlab("Year") + ylab(load_label()) +
      theme(legend.position="none")
  })
  
  # Render the table of clicked data points
  output$PlotClickInfo <- renderTable({
    nearPoints(load_final(), input$plot_click)
  })
  
  # Reactive expression for the geographic data of clicked points
  load_geo <- reactive({
    semi_join(load_geo_init(), nearPoints(load_final(), input$plot_click), by = "CTUID") %>%
      st_sf()
  })
  
  # Render the Leaflet map
  output$MyMap <- renderLeaflet({
    
    palCT <- colorBin("Reds", domain = load_geo()$load, bins = bins())
    labelsCT <- sprintf(paste0(load_label(), "%f<br/>CTUID: %s"),
                        round(load_geo()$load, digits = 4), load_geo()$CTUID) %>%
      lapply(htmltools::HTML)
    
    temp_map <- leaflet(load_geo())
    
    # Add provider tiles and base (grey) map
    temp_map <-
      addProviderTiles(temp_map, providers$CartoDB.Positron) %>%
      addPolygons(data = CTUID_geo_map, color = "grey", weight = 1,
                  opacity = 1, fillOpacity = 0.65)
    
    # Add polygons of clicked data points with styling and labels
    if (!is.null(input$plot_click)) {
      temp_map <-
        addPolygons(temp_map, fillColor = ~palCT(load_geo()$load),
                    color = "white", weight = 1, opacity = 1, fillOpacity = 0.65,
                    highlightOptions = highlightOptions(color = "grey", weight = 3,
                                                        bringToFront = TRUE),
                    label = labelsCT,
                    labelOptions = labelOptions(textsize = "15px")) %>%
        addLegend(pal = palCT, values = load_geo()$load, opacity = 0.9,
                  title = load_label(), position = "bottomleft")
    }
    
    temp_map
    
  })
}

shinyApp(ui = ui, server = server)
