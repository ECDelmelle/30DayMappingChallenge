library(tidyverse)
library(sf)
library(ggplot2)
library(tidycensus)
library(tmap)
library(here)
library(ggthemes)
library(mapsf)
library(RColorBrewer)


bball <- st_read("Data/Major_League_Baseball_Stadiums.shp")
wins <- read.csv("Data/wins.csv")
joined <- left_join(bball, wins)

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


mf_shadow(us)
mf_map(us, 
       border = "white",
       col = "darkgrey",
       add = TRUE)
# plot baseball values
mf_map(
  x = joined,
  var = c("Appear","Appear"),
  border = "black",
  type = "prop_choro",
  breaks = c(19,39,40),
  pal = "Blues",
  nbreaks = 3,
  inches = 0.25,
  leg_pos = "bottomleft2",
  leg_title = "World Series Appearances"
)

# layout
mf_layout(
  title = "World Series Appearances by Franchise",
  credits = paste0(
    "Author: Elizabeth Delmelle\n",
    "@Weizman_MUSA, Master of Urban Spatial Analytics\n",
    "University of Pennsylvania\n",
    "Sources: Wikipedia\n",
    "mapsf",
    packageVersion("mapsf")
  )
)
mf_annotation(c(1849796, 2130000),
              txt = "Yankees\n(40)",
              pos = "bottomright", cex = 1, font = 1.0
              
)

mf_annotation(c(-2028789, 1460539),
              txt = "Dodgers\n(21)",
              pos = "bottomleft", cex = 1, font = 1.0
             
)


mf_annotation(c(-2272832, 1998200),
              txt = "Giants\n(20)",
              pos = "topright", cex = 1, font = 1.0
              
)

