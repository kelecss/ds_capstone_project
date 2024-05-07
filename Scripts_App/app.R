# Script Name: pool_data_01.R
# Description: This script taps into the Zurich Open Data API to extract data on the status of all public outdoor pools in Zurich.
# Author: Kerim Lengwiler
# Date Created: 2024_05_04
# Date Updated: 2024_05_04

# Loading packages

# Loading packages

library(shiny)
library(httr)
library(XML)
library(bslib)
library(leaflet)
library(tidyverse)
library(sf)
library(shinydashboard)

url <- "https://www.stadt-zuerich.ch/stzh/bathdatadownload"

# Make the request
pool_response <- GET(url = url, query = list()) 
http_status(pool_response)

xml_data <- xmlParse(rawToChar(pool_response$content))


pool <- getNodeSet(xml_data, "//bath")

# Extract data for each bath
pool_details <- lapply(pool, function(pool) {
  title <- xmlValue(getNodeSet(pool, "./title")[[1]])
  temperatureWater <- xmlValue(getNodeSet(pool, "./temperatureWater")[[1]])
  poiid <- xmlValue(getNodeSet(pool, "./poiid")[[1]])
  dateModified <- xmlValue(getNodeSet(pool, "./dateModified")[[1]])
  openClosedTextPlain <- xmlValue(getNodeSet(pool, "./openClosedTextPlain")[[1]])
  urlPage <- xmlValue(getNodeSet(pool, "./urlPage")[[1]])
  pathPage <- xmlValue(getNodeSet(pool, "./pathPage")[[1]])
  
  # Return a list of bath details
  list(Title = title, Wassertemperatur = temperatureWater, ID = poiid, 
       Update = dateModified, Status = openClosedTextPlain, 
       URL_Page = urlPage, Path_Page = pathPage)
})

# Convert the list to a data frame for easier manipulation and viewing
df_pools <- do.call(rbind.data.frame, pool_details)

if (class(df_pools$Wassertemperatur) %in% c("factor", "character")) {
  df_pools$Wassertemperatur <- as.numeric(as.character(df_pools$Wassertemperatur))
}

## Getting the Locations

df_pools_coordinates <- data.frame(
  Title = c("Flussbad Au-Höngg","Flussbad Oberer Letten","Flussbad Unterer Letten", "Flussbad Unterer Letten Flussteil", "Frauenbad Stadthausquai", "Freibad Allenmoos","Freibad Auhof",
            "Freibad Dolder", "Freibad Heuried", "Freibad Letzigraben", "Freibad Seebach", "Freibad Zwischen den Hölzern", "Hallenbad Bläsi", "Hallenbad Bungertwies", "Hallenbad City",
            "Hallenbad Leimbach", "Hallenbad Oerlikon", "Männerbad Schanzengraben ", "Seebad Enge", "Seebad Katzensee", "Seebad Utoquai", "Strandbad Mythenquai",
            "Strandbad Tiefenbrunnen","Strandbad Wollishofen ", "Wärmebad Käferberg" ),
  Latitude = c(47.39910, 47.38532, 47.39005, 47.38872, 47.36839, 47.40549, 47.40876, 47.37548, 47.36778, 47.37851, 47.42352, 47.40904, 47.40133, 47.37201, 47.37209, 47.32652,
               47.41023, 47.37122, 47.36170, 47.42844, 47.36176, 47.35451, 47.35239, 47.34112, 47.39942),
  Longitude = c(8.489321, 8.534956, 8.528608, 8.530987, 8.542030, 8.539013, 8.571462, 8.576269, 8.505399, 8.498366, 8.548195, 8.469622, 8.501876, 8.560206, 8.532868, 8.513683, 8.556734, 
                8.532703, 8.536701, 8.495608, 8.547013, 8.534713, 8.557461, 8.537336, 8.518000))


# Merging
df_pools <- merge(df_pools, df_pools_coordinates, by = "Title")

###SHINY APP

# Setting up User Interface (ui)

ui <- dashboardPage(
  dashboardHeader(title = "Pool Dashboard"),
  dashboardSidebar(
    sliderInput("temperatur", "Wassertemperatur", min = min(df_pools$Wassertemperatur, na.rm = TRUE), 
                max = max(df_pools$Wassertemperatur, na.rm = TRUE), value = c(min(df_pools$Wassertemperatur, na.rm = TRUE), 
                                                                              max(df_pools$Wassertemperatur, na.rm = TRUE))),
    selectInput("status", "Status", choices = c("Alle", "offen", "geschlossen")),
    selectInput("title", "Name des Bades", choices = c("Alle", "Please select")),
    width = 250
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Ensure the map fills the height and adjusts for the header height */
        .leaflet-container {
          height: calc(100vh - 80px) !important; /* Adjust if your header is a different height */
        }
      "))
    ),
    leafletOutput("map", width = "100%", height = "100%")
  )
)

# Setting up server element


server = function(input, output, session) {
  
  observe({
    sorted_titles <- sort(unique(df_pools$Title))
    updateSelectInput(session, "title", choices = c("Alle", sorted_titles))
  })
  
  map_df = reactive({
    temp_filtered <- df_pools %>%
      filter(Wassertemperatur >= input$temperatur[1], Wassertemperatur <= input$temperatur[2])
    
    status_filtered <- if (input$status != "Alle") {
      temp_filtered %>%
        filter(Status == input$status)
    } else {
      temp_filtered
    }
    
    title_filtered <- if (input$title != "Alle") {
      status_filtered %>%
        filter(Title == input$title)
    } else {
      status_filtered
    }
    
    title_filtered %>%
      filter(!is.na(Longitude), !is.na(Latitude)) %>%
      st_as_sf(coords = c("Longitude", "Latitude")) %>%
      st_set_crs(4326)
  })
  
  output$map = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 8.5417, lat = 47.3769, zoom = 12) %>%
      addCircleMarkers(
        data = map_df(),
        popup = ~paste("<b>Bad:</b>", Title, "<br><strong>Wassertemperatur:</strong>", Wassertemperatur, "°C",
                       "<br><b>Status</b>", Status, "<br>Zuletzt aktualisiert:", Update),
        radius = 8,
        color = '#007BFF',
        fillOpacity = 0.7)
  })
}

shinyApp(ui, server)