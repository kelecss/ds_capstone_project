library(shiny)
library(leaflet)
library(httr)
library(jsonlite)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$script('
      function getLocation() {
        if (navigator.geolocation) {
          navigator.geolocation.getCurrentPosition(showPosition);
        } else {
          Shiny.onInputChange("geolocation", "Geolocation is not supported by this browser.");
        }
      }
      function showPosition(position) {
        var lat = position.coords.latitude;
        var lng = position.coords.longitude;
        Shiny.onInputChange("geolocation", {lat: lat, lng: lng});
      }
      $(document).ready(function() {
        getLocation();
      });
    ')
  ),
  leafletOutput("map"),
  verbatimTextOutput("route_info")
)

# Define Server
server <- function(input, output, session) {
  # Example pool locations
  pools <- data.frame(
    name = c("Pool A", "Pool B", "Pool C"),
    lat = c(47.3769, 47.3799, 47.3840),
    lng = c(8.5417, 8.5440, 8.5480)
  )
  
  # Initialize the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = pools, ~lng, ~lat, popup = ~name, layerId = ~name)
  })
  
  # Update map with user's geolocation
  observeEvent(input$geolocation, {
    user_loc <- input$geolocation
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(data = pools, ~lng, ~lat, popup = ~name, layerId = ~name) %>%
      addMarkers(lng = user_loc$lng, lat = user_loc$lat, popup = "You are here", layerId = "user")
    session$userData$user_loc <- c(user_loc$lat, user_loc$lng)
  })
  
  # Observe marker clicks for pools
  observeEvent(input$map_marker_click, {
    marker <- input$map_marker_click
    if (!is.null(session$userData$user_loc) && marker$id != "user") {
      user_loc <- session$userData$user_loc
      pool_loc <- pools[pools$name == marker$id, ]
      
      # Get route from OpenRouteService
      ors_api_key <- "YOUR_OPENROUTESERVICE_API_KEY"
      ors_url <- "https://api.openrouteservice.org/v2/directions/driving-car"
      response <- GET(
        ors_url,
        query = list(
          api_key = ors_api_key,
          start = paste(user_loc[2], user_loc[1], sep = ","),
          end = paste(pool_loc$lng, pool_loc$lat, sep = ",")
        )
      )
      route <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
      
      # Extract coordinates from the route
      coordinates <- do.call(rbind, route$features[[1]]$geometry$coordinates)
      route_line <- data.frame(lng = coordinates[, 1], lat = coordinates[, 2])
      
      # Add route to the map
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolylines(data = route_line, lng = ~lng, lat = ~lat, color = "blue")
      
      # Show route info
      output$route_info <- renderText({
        distance <- route$features[[1]]$properties$segments[[1]]$distance / 1000 # in km
        duration <- route$features[[1]]$properties$segments[[1]]$duration / 60   # in minutes
        paste("Distance:", round(distance, 2), "km\nDuration:", round(duration, 2), "minutes")
      })
    }
  })
}

# Run the application
shinyApp(ui, server)