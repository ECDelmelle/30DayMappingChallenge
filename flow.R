library(here)
library(tidyverse)
library(sf)
library(tidycensus)
library(leaflet)
library(leaflet.extras)


bball <- read.csv('Data/wins.csv')
stadium <- read_sf('Data/Major_League_Baseball_Stadiums.shp')

joined <- left_join(bball, stadium)

bball <- bball %>% 
  mutate(LEAGUE = ifelse(CITY == 'Houston','American', LEAGUE))%>%
  mutate(TEAM = ifelse(TEAM == 'Cleveland Indians', 'Cleveland Guardians', TEAM))

us <- get_decennial(
  geography = "state", 
  year = 2010,
  variables = "P001001",
  geometry = TRUE
)
cont_us <- us%>% filter(NAME != 'Alaska' & NAME != 'Hawaii' & NAME != 'Puerto Rico')

joined <- st_transform(joined, crs = 'EPSG:5070' )
us <- st_transform(cont_us, crs = 'EPSG:5070' )


philly <- st_coordinates(stadium[3,])
kc <- st_coordinates(stadium[10,])
oak <- st_coordinates(stadium[29,])
# Example data with latitude and longitude for stops
locations <- data.frame(
  name = c("Start", "Stop1", "End"),
  lat = c(39.90618, 39.05164, 37.75161),
  lon = c(-75.16647, -94.48043, -122.2006)
)

locations$label <- 
# Create a leaflet map
mymap <- leaflet() %>%
  addTiles() 

# Add the route with multiple stops
mymap <- addAwesomeMarkers(
  map = mymap,
  lng = locations$lon,
  lat = locations$lat,
  label = locations$name,
  icon = NULL
)

# Add a routing line
mymap <- addPolylines(
  map = mymap,
  lng = locations$lon,
  lat = locations$lat,
  label = locations$name
)

mymap <- addArrowhead(
  map = mymap,
  lng = locations$lon,
  lat = locations$lat,
  label = "Arrowhead",  # Change the label for the arrowhead
  angle = 30,           # Specify the angle of the arrowhead
  length = 20           # Specify the length of the arrowhead
)

# Display the map
mymap

