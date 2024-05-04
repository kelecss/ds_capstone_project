# Data Mining in R #
# University of Lucerne #
# Capstone Project #
# Kerim Lengwiler #

#GOALS
# 1. Get Data from the API of the City of Zurich regarding the status and temperature
#   of public swimming pools: https://www.stadt-zuerich.ch/stzh/bathdatadownload

# 2. Create a Shiny App (Web Application) that displays the data in a user-friendly way:
#    a) A map of Zurich with the locations of the swimming pools
#    b) Information about the status and temperature of the swimming pools

# 3. Manage to automatically update the data every 1 hour and display the updated data in the shiny app

# 4. Optional:   


install.packages("httr")
install.packages("XML")
install.packages("shiny")
library(httr)
library(XML)
library(tidyr)


url <- "https://www.stadt-zuerich.ch/stzh/bathdatadownload"

# Make the request
bath_response <- GET(url = url, query = list()) 
http_status(bath_response)

xml_data <- xmlParse(rawToChar(bath_response$content))


baths <- getNodeSet(xml_data, "//bath")

# Extract data for each bath
bath_details <- lapply(baths, function(bath) {
  title <- xmlValue(getNodeSet(bath, "./title")[[1]])
  temperatureWater <- xmlValue(getNodeSet(bath, "./temperatureWater")[[1]])
  poiid <- xmlValue(getNodeSet(bath, "./poiid")[[1]])
  dateModified <- xmlValue(getNodeSet(bath, "./dateModified")[[1]])
  openClosedTextPlain <- xmlValue(getNodeSet(bath, "./openClosedTextPlain")[[1]])
  urlPage <- xmlValue(getNodeSet(bath, "./urlPage")[[1]])
  pathPage <- xmlValue(getNodeSet(bath, "./pathPage")[[1]])
  
  # Return a list of bath details
  list(Title = title, TemperatureWater = temperatureWater, POIID = poiid, 
       DateModified = dateModified, Status = openClosedTextPlain, 
       URLPage = urlPage, PathPage = pathPage)
})

# Convert the list to a data frame for easier manipulation and viewing
df_baths <- do.call(rbind.data.frame, bath_details)

# Print the data frame
View(df_baths)

