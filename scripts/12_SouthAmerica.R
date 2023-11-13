library(sf)
library(mapsf)
library(giscoR)
library(dplyr)
library(here)

sa_shp <- gisco_get_countries(
  year = "2020",
  epsg = '4326',
  cache = TRUE,
  update_cache = FALSE,
  cache_dir = NULL,
  verbose = FALSE,
  resolution = "20",
  spatialtype = "RG",
  country = NULL,
  region = "Americas"
)

sa_select<- sa_shp %>% filter(NAME_ENGL=="Argentina"|NAME_ENGL=="Colombia"|
                                NAME_ENGL== "Curaçao"|NAME_ENGL=="Bolivia"|
                                NAME_ENGL=="Brazil"|NAME_ENGL=="Chile"|
                                NAME_ENGL== "Ecuador"|NAME_ENGL=="Peru"|
                                NAME_ENGL== "Uruguay"|NAME_ENGL=="Venezuela"|
                                NAME_ENGL=="Trinidad and Tobago"|NAME_ENGL=="Paraguay"|
                                NAME_ENGL== "Suriname"|NAME_ENGL=="Guyana")

sa_select <- sa_select%>%rename(country = "Country")
data <- read.csv('Data/southamerica.csv')
sa_joined <- left_join(sa_select, southamerica)

sa_joined <- sa_joined %>% st_transform(2317)

sa_joined <- sa_joined %>% replace(is.na(.), 0)
mf_theme("nevermind")
mf_shadow(sa_joined, col = "black")
mf_choro(x = sa_joined, 
         var = "Count",
         breaks = c(0,6,14,106),
         pal = "Reds 2",
         border = "grey",
         leg_title = "Number of Players",
         leg_val_rnd = 0,
         leg_pos = "bottomright",
         add= TRUE)


mf_layout(
  scale = FALSE,
  credits = paste0(
    "Author: Elizabeth C. Delmelle\n",
    "Master of Urban Spatial Analytics\n",
    "University of Pennsylvania\n",
    "#30DayMappingChallenge - Day 12 - South America\n",
    "Source: Wikipedia\n",
    "mapsf ",
    packageVersion("mapsf")
  ),
)
mf_title(txt = "South American Origins of Current MLB Players", pos = 'left')


