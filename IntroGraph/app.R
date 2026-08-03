library(shiny)
library(sf)
library(plotly)
library(dotenv)
library(DBI)
library(RPostgres)
library(tidyverse)
library(ggplot2)
library(scales)
library(GGally)
library(patchwork)
library(ggrepel)
library(ggcorrplot)
library(car)
library(tigris) # Added so or_counties loads properly

#-------------------
# LOAD DATA
#-------------------

geo_data <- st_read("OR_avas.geojson")

# Download/cache Oregon county borders for background map layer
or_counties <- counties(state = "Oregon", class = "sf")


# ---------------------------
# UI
# ---------------------------

ui <- fluidPage(
  #titlePanel("Interactive Oregon AVAs"),
  tabsetPanel(
    
    # TAB 1: Map & Controls
    tabPanel("AVA & County Map",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput(
                   inputId  = "selected_ava",
                   label    = "Select an AVA:",
                   choices  = c("All AVAs" = "All", sort(unique(geo_data$name))),
                   selected = "All"
                 )
               ),
               mainPanel(
                 width = 9,
                 plotlyOutput("avaMap", height = "700px")
               )
             )
    )#,
    #tabPanel()
  )
)

# ---------------------------
# SERVER LOGIC
# ---------------------------

server <- function(input, output, session) {
  
  # 1. REACTIVE DATA FILTER (Directly inside server)
  filtered_geo <- reactive({
    if (input$selected_ava == "All") {
      geo_data
    } else {
      geo_data %>% filter(name == input$selected_ava)
    }
  })
  
  # 2. MAP RENDER
  output$avaMap <- renderPlotly({
    my_map <- ggplot() +
      geom_sf(data = or_counties, fill = "grey95", color = "darkgrey") +
      geom_sf_text(data = or_counties, 
                   aes(label = NAME, text = NAME),  
                   size = 2.5, 
                   color = "grey50", 
                   fontface = "italic",
                   nudge_x = 0.05,
                   nudge_y = 0.05) +
      
      geom_sf(data = filtered_geo(), aes(text = name), fill = "lightblue", color = "blue", size = 0.2, alpha = 0.6) +
      
      theme_minimal() +
      labs(title = if (input$selected_ava == "All") "Oregon AVAs & Counties" else paste("AVA:", input$selected_ava)) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank()
      )
    
    gg <- ggplotly(my_map, tooltip = "text")
    
    # Hover fix loop
    for (i in seq_along(gg$x$data)) {
      if (is.null(gg$x$data[[i]]$mode) || gg$x$data[[i]]$mode != "text") {
        gg$x$data[[i]]$hoveron <- "fills"
        gg$x$data[[i]]$textposition <- "none"
        gg$x$data[[i]]$hoverinfo <- "text"
      }
    }
    
    gg
  })
} 

shinyApp(ui, server)
