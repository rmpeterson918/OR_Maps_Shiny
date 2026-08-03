library(shiny)
library(sf)
library(plotly)
library(tidyverse)
library(ggplot2)
library(scales)
library(ggrepel)
library(tigris)

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

    # TAB 2: Static Publication Map with ggrepel
    tabPanel("Static Detailed Map",
             mainPanel(
               width = 12,
               plotOutput("avaMapStatic", height = "800px")
             )
    ),
    
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
    
    for (i in seq_along(gg$x$data)) {
      gg$x$data[[i]]$hoverinfo <- "none"
    }
    
    gg
  })
  
  # 3. TAB 2: STATIC MAP (Native ggplot + ggrepel)
  output$avaMapStatic <- renderPlot({
    ggplot() +
      # Layer 1: Oregon Counties (Background)
      geom_sf(data = or_counties, fill = "grey95", color = "darkgrey") +
      geom_sf_label(data = or_counties, aes(label = NAME), 
                    size = 2.5, 
                    color = "grey40", 
                    fontface = "italic",
                    fill = alpha("white", 0.6), 
                    label.size = NA,            
                    label.padding = unit(0.05, "lines")) +
      
      # Layer 2: AVAs
      geom_sf(data = geo_data, fill = "lightblue", color = "blue", size = 0.2, alpha = 0.5) +
      
      # Layer 3: Repelled AVA Labels
      geom_label_repel(
        data = geo_data,
        aes(label = name, geometry = geometry),
        stat = "sf_coordinates",
        size = 3, 
        color = "darkblue", 
        fill = "white", 
        alpha = 0.8,
        min.segment.length = 0
      ) +
      
      # Styling
      theme_minimal() +
      labs(title = "Oregon AVAs & Counties (Static View)") +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank()
      )
  }, res = 96)
    
} 

shinyApp(ui, server)
