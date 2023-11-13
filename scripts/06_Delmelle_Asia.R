library(sf)
library(mapsf)
library(giscoR)

asia_shp <- gisco_get_countries(
  year = "2020",
  epsg = "4326",
  cache = TRUE,
  update_cache = FALSE,
  cache_dir = NULL,
  verbose = FALSE,
  resolution = "20",
  spatialtype = "RG",
  country = NULL,
  region = "Asia"
)

asia_shp <- asia_shp %>% rename(Country=NAME_ENGL)
  
asia_select<-asia_joined%>%filter(Country == 'Japan'|Country == 'North Korea'| 
                                    Country == "South Korea"| Country == 'China')

data <- read.csv('asia.csv')

asia_joined <- left_join(asia_shp, data)

#Replace NAs with 0
asia_joined <- asia_joined %>% replace(is.na(.), 0)

#Make a map

mf_theme("dark")
mf_shadow(asia_select)
mf_choro(x = asia_select, 
      var = "Count",
      breaks = c(0,4,7,9),
      pal = "Reds 2",
      border = "grey",
      leg_title = "Number of Players",
      leg_val_rnd = 0,
      leg_pos = "bottomright")
mf_label(
  x = asia_select,
  var = "Country",
  col = 'black',
  halo = TRUE,
  bg = "white",
  overlap = FALSE
)

mf_layout(
  scale = FALSE,
  title = "Asian Countries of Origin of Current MLB Players",
  credits = paste0(
    "Author: Elizabeth C. Delmelle\n",
    "Master of Urban Spatial Analytics\n",
    "University of Pennsylvania\n",
    "#30DayMappingChallenge - Day 6 - Asia\n",
    "Source: Wikipedia\n",
    "mapsf ",
    packageVersion("mapsf")
  ),
)






