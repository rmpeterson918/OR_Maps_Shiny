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

geo_data <- st_read("/Users/rebekahpeterson/Documents/DS 510/OR_avas.geojson")

# Download/cache Oregon county borders for background map layer
or_counties <- counties(state = "Oregon", class = "sf")

# Safe DB Port fallback
port_val <- Sys.getenv("DB_PORT")
if (port_val == "") port_val <- "5432"

con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = Sys.getenv("DB_NAME"),
  host     = Sys.getenv("DB_HOST"),
  port     = as.integer(port_val),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  connect_timeout = 10
)

# Load both analysis panels
regional  <- dbReadTable(con, "analysis_panel_regional")   %>% as_tibble()
statewide <- dbReadTable(con, "analysis_panel_statewide")  %>% as_tibble()

regional <- regional %>%
  mutate(
    yield_best = coalesce(yield_per_acre, yield_derived),
    has_reported_yield = !is.na(yield_per_acre)
  )
statewide <- statewide %>%
  mutate(
    yield_best = coalesce(yield_per_acre, yield_derived),
    has_reported_yield = !is.na(yield_per_acre)
  )

# Collapse to region-year
ry <- regional %>%
  distinct(region_label, yr, gdd_apr_sep, gdd_apr_oct, gdd_veraison_harvest,
           frost_days_annual, frost_days_spring, heat_stress_days,
           coldest_spring_night_c, winter_tmin_jan_feb, summer_tmax_mean,
           diurnal_range_ripening, gs_ppt_mm, oct_ppt_mm, sep_oct_ppt_mm,
           spring_ppt_mm, annual_ppt_mm, vpd_max_summer, vpd_max_ripening)

ry_anom <- ry %>%
  filter(!region_label %in% c("Eastern Oregon", "Other Region", "Other Oregon")) %>%
  group_by(region_label) %>%
  mutate(gdd_anom = gdd_apr_sep - mean(gdd_apr_sep, na.rm = TRUE)) %>%
  ungroup()

sw_climate <- statewide %>%
  distinct(yr, gdd_apr_sep, gdd_apr_oct, frost_days_annual, frost_days_spring,
           heat_stress_days, summer_tmax_mean, oct_ppt_mm, annual_ppt_mm,
           vpd_max_summer, diurnal_range_ripening) %>%
  arrange(yr) %>%
  mutate(gdd_rolling10 = zoo::rollmean(gdd_apr_sep, 10, fill = NA, align = "right"))

sw_climate <- sw_climate %>%
  mutate(gdd_anom = gdd_apr_sep - mean(gdd_apr_sep, na.rm = TRUE),
         gdd_rolling10_anom = gdd_rolling10 - mean(gdd_apr_sep, na.rm = TRUE))

dbDisconnect(con)

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
