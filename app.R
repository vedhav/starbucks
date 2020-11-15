library(shiny)
library(shinyWidgets)
library(shinyjs)
library(bs4Dash)
library(leaflet)
library(leaflet.extras)
library(DT)
library(tidyverse)
library(rgdal)

popUpWindow <- function(
  popUpText, title = NULL, footer = NULL, easyClose = TRUE,
  color = "#333", bg_color = "#f7f7f7") {
  tags$div(
    class = "showmodal",
    showModal(
      modalDialog(
        style = paste0('color: ', color, '; background-color: ', bg_color),
        title = title, tags$div(HTML(popUpText), align = "center"), footer = footer, easyClose = easyClose
      )
    )
  )
}

starbucks <- read_csv("starbucks.csv") %>% left_join(read_csv("us_states.csv")) %>% mutate(city = paste0(city, ", ", state_code))
starbucks <- starbucks[complete.cases(starbucks), ]
starbucks_state_count <- starbucks %>% group_by(state_name) %>% summarise(store_count = n()) %>% ungroup()
starbucks <- starbucks %>% left_join(starbucks_state_count) %>%
  mutate(state_name = paste0(state_name, " (", store_count, " stores)"))
state_names <- unique(starbucks$state_name)
state_cities <- starbucks %>% select(c(state_name, city)) %>% distinct()
cities_list <- list()
for (state in state_names) {
  cities_in_state <- state_cities %>% filter(state_name == state) %>% pull(city)
  cities_list <- c(cities_list, list(state = cities_in_state))
}
names(cities_list) <- state_names
ny_cities <- state_cities %>% filter(str_detect(state_name, "New York")) %>% pull(city)
states <- rgdal::readOGR("us_states.json")

pal <- colorNumeric(
  palette = "Blues",
  domain = states@data$population
)

ui <- bs4DashPage(
  sidebar_collapsed = FALSE,
  controlbar_overlay = FALSE,
  title = "Starbucks locations",
  conditionalPanel(
    condition="$('html').hasClass('shiny-busy')",
    tags$div(
      style = "position: fixed;top: 250px; left: 0px; width: 100%;
      padding: 5px 0px 5px 0px; text-align: center; font-weight: bold;
      font-size: 300%; color: #ffffff; background-color:'transparent'; z-index: 105;",
      tags$img(src = "loading_icon.svg", height = "200px", width = "200px")
    )
  ),
  tags$head(tags$link(rel = "shortcut icon", type = "image/png", href = "favicon.png")),
  fluidPage(
    useShinyjs(),
    fluidRow(
      style = "margin: 20px 0px 0px 20px",
      fluidRow(br()),
      column(
        3,
        fluidRow(
          column(
            12, style = "z-index: 1002",
            pickerInput(
              "state_in", "State", state_names, width = "100%", "New York (426 stores)",
              multiple = TRUE, options = pickerOptions(liveSearch = TRUE, maxOptions = 5, actionsBox = TRUE)
            )
          ),
          column(
            12, style = "z-index: 1001",
            pickerInput(
              "city_in", "City", cities_list, ny_cities, multiple = TRUE, width = "100%",
              options = pickerOptions(liveSearch = TRUE)
            )
          ),
          column(
            12, align = "center",
            switchInput(
              "show_population", onLabel = "Show population", offLabel = "Hide population",
              value = TRUE, width = "100%"
            )
          ),
          tags$div(id = "click_info", align = "center", "Click the states on the map to display more info"),
          column(12, uiOutput("num_of_stores_ui")),
          column(12, uiOutput("total_revenue_ui")),
          column(12, uiOutput("average_revenue_ui")),
          tags$div("Note: The revenue show are random numbers and not the actual revenue of starbucks")
        )
      ),
      column(
        9,
        bs4TabCard(
          title = "",
          elevation = 2,
          id = "star_tabs",
          width = 12,
          collapsible = FALSE, 
          closable = FALSE,
          bs4TabPanel(
            tabName = "Starbucks Geomap",
            column(12, align = "center", leafletOutput("geo_plot", height = "80vh"))
          ),
          bs4TabPanel(
            tabName = "Starbucks Revenue",
            column(12, align = "center", uiOutput("revenue_ui"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  render_value_box <- function(data, state) {
    output$num_of_stores_ui <- renderUI({
        bs4ValueBox(
          value = nrow(data),
          subtitle = "Number of stores",
          status = "primary",
          icon = "store",
          width = 12,
          footer = state
        )
      })
      output$total_revenue_ui <- renderUI({
        bs4ValueBox(
          value = scales::dollar(sum(data$revenue)),
          subtitle = "Total Revenue",
          status = "primary",
          icon = "dollar-sign",
          width = 12,
          footer = state
        )
      })
      output$average_revenue_ui <- renderUI({
        bs4ValueBox(
          value = scales::dollar(mean(data$revenue)),
          subtitle = "Average Revenue",
          status = "primary",
          icon = "money-bill",
          width = 12,
          footer = state
        )
      })
  }
  observeEvent(input$state_in, {
    current_cities <- state_cities %>% filter(state_name %in% input$state_in) %>% pull(city)
    updatePickerInput(session, "city_in", selected = current_cities)
  })
  output$geo_plot <- renderLeaflet({
    ny <- starbucks %>% filter(city %in% ny_cities)
    render_value_box(ny, "New York")
    leaflet(ny) %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      setView(-97, 39, 4) %>%
      addMarkers(
        lng = ~lon, lat = ~lat, clusterOptions = markerClusterOptions(),
        label = ~address
      ) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Satellite")
      )
  })
  observeEvent(input$city_in, {
    plot_data <- starbucks %>% filter(city %in% input$city_in)
    leafletProxy("geo_plot", data = plot_data) %>%
      clearMarkerClusters() %>%
      addMarkers(
        lng = ~lon, lat = ~lat, clusterOptions = markerClusterOptions(),
        label = ~address
      )
  })
  observeEvent(input$show_population, {
    if (input$show_population) {
      leafletProxy("geo_plot", data = states) %>%
        clearShapes() %>% clearControls() %>%
        addPolygons(layerId = ~NAME, stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.75,
          fillColor = ~pal(population),
          label = ~paste0(NAME, ": ", population)) %>%
        addLegend("bottomright", pal = pal, values = ~population,
          title = "Population", opacity = 1, na.label = NULL)
    } else {
      leafletProxy("geo_plot", data = states) %>%
        clearShapes() %>% clearControls()
    }
  })
  observeEvent(input$geo_plot_shape_click, {
    shinyjs::hide("click_info")
    selected_data <- starbucks %>% filter(str_detect(state_name, input$geo_plot_shape_click$id))
    if (nrow(selected_data) != 0) {
      shinyjs::show("num_of_stores_ui")
      shinyjs::show("total_revenue_ui")
      shinyjs::show("average_revenue_ui")
      render_value_box(selected_data, input$geo_plot_shape_click$id)
    } else {
      shinyjs::hide("num_of_stores_ui")
      shinyjs::hide("total_revenue_ui")
      shinyjs::hide("average_revenue_ui")
    }
  })
  output$revenue_ui <- renderUI({
    fluidRow(
      bs4ValueBox(
        value = 150,
        subtitle = "New orders",
        status = "primary",
        icon = "shopping-cart"
      ),
      bs4ValueBox(
        elevation = 4,
        value = "53%",
        subtitle = "New orders",
        status = "danger",
        icon = "cogs"
      ),
      bs4ValueBox(
        value = "44",
        subtitle = "User Registrations",
        status = "warning",
        icon = "sliders"
      ),
      bs4ValueBox(
        value = "53%",
        subtitle = "Bounce rate",
        status = "success",
        icon = "database"
      ),
      fluidRow("This tab is still under construction!")
    )
  })
}

shinyApp(ui, server)


