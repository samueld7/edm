library(shiny)
library(DT)
library(dplyr)
library(geosphere)
library(readr)

data <- read_delim("C:/Users/jorge/OneDrive/Escritorio/EDM/valenbisi-disponibilitat-valenbisi-dsiponibilidad.csv",
                   delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

data <- data %>%
  mutate(
    Latitude = sapply(strsplit(as.character(geo_point_2d), ","), function(x) as.numeric(x[1])),
    Longitude = sapply(strsplit(as.character(geo_point_2d), ","), function(x) as.numeric(x[2]))
  )

# Define the UI
ui <- fluidPage(
  titlePanel("Valenbisi Station Information"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("user_lat", "Your Latitude:", value = NULL),
      numericInput("user_lng", "Your Longitude:", value = NULL),
      actionButton("update", "Find Nearest Station")
    ),
    
    mainPanel(
      DTOutput("station_info_table")
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Calculate the distance between user's location and each station
  station_distances <- reactive({
    if (!is.null(input$user_lat) && !is.null(input$user_lng)) {
      user_location <- c(input$user_lng, input$user_lat)
      data$Distance <- apply(data[, c("Longitude", "Latitude")], 1, function(x) {
        distm(user_location, x)
      })
    }
    data
  })
  
  # Update the table with station information based on user's location
  observeEvent(input$update, {
    filtered_data <- station_distances()
    filtered_data <- filtered_data[order(filtered_data$Distance), c("Direccion", "Activo","Espacios_libres", "Distance")]
    output$station_info_table <- renderDataTable({
      datatable(filtered_data, options = list(pageLength = 10))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)