library(ggplot2)
library(sf)
library(dplyr)
library(tmap)
library(tidycensus)
library(tigris)
library(mapboxapi)
library(here)
library(stars)
library(gstat)
library(ggthemes)
library(terra)
library(raster)
options(tigris_use_cache = TRUE)

##This originally started off as a script to create an isochrone map, but it ended up becoming a bigger endeavor and the map looked better without the isochrones in the end. 
#So this could be either day 2 or 3 code.


us <- get_decennial(
  geography = "state", 
  year = 2010,
  variables = "P001001",
  geometry = TRUE
)

#get us county shapefiles
counties <- counties(state = NULL, cb = FALSE, resolution = "500k", year = NULL)

#load stadiums
bball <- read_sf("Data/Major_League_Baseball_Stadiums.shp")

#reproject to albers equal area (though maybe equal distance would be better for isochrone)
counties <- st_transform(counties, crs = 'EPSG:5070' )
bball <- st_transform(bball, crs = 'EPSG:5070' )
us <- st_transform(us, crs = 'EPSG:5070' )

cont_us <- us%>% filter(NAME != 'Alaska' & NAME != 'Hawaii' & NAME != 'Puerto Rico')
#convert counties to centroids
county_centroid <- st_centroid(counties)

#don't try this at home
times <- mb_matrix(county_centroid, bball, allow_large_matrix = TRUE)
write.csv(times, "times.csv")
times <-read.csv("times.csv")
times<-times[, -1]
times <- as.matrix(times)

#Find minimum travel time and apply to county centroids
min_time <- apply(times, 1, min)
county_centroid$time <- min_time

min_time <- apply(times, 1, min)
counties$time <- min_time

#Filter out HI, AK, Guam etc. 
cont_us <- county_centroid%>% filter(STATEFP != '15' & STATEFP != '66' & STATEFP != '60' & STATEFP != '69'& 
                                        STATEFP != '02'& STATEFP != '78' & STATEFP != '72')

cont_us_poly <- counties%>% filter(STATEFP != '15' & STATEFP != '66' & STATEFP != '60' & STATEFP != '69'& 
                                       STATEFP != '02'& STATEFP != '78' & STATEFP != '72')

# do the interpolation
# first create a grid around the spatial extent of cont_us_poly
grid <- st_bbox(cont_us_poly)%>% st_as_stars(dx = 50000)%>%st_crop(cont_us_poly)
grid = st_as_sf(grid)

#interpolate to grid
i <- idw(time~1, cont_us, grid)

#convert from min to hours
i$hours <- (i$var1.pred/60)

#Convert gridded polygons to raster.
ext <- extent(i)
raster_grid <- raster(ext, res = 200)
rasterized_grid <- rasterize(i, raster_grid, field = "hours")

##Calculate contours from raster
contour_lines <- contour(rasterized_grid, by = 0.5)

cl <- rasterToContour(rasterized_grid)


cl = as.contour(rasterized_grid) |> 
  st_as_sf()

cl <- st_as_sf(cl)



##Make the map
iso_map <- tm_shape(i)+
  tm_polygons(fill = "hours", col = NA, fill.legend = tm_legend(title = "Hours", orientation = "landscape"))+
  tm_layout(frame = FALSE, legend.frame = FALSE)

iso_map <- iso_map + tm_shape(cont_us)+
  tm_polygons(fill = NA, col = "white", lwd = 0.75)

iso_map <- iso_map + tm_shape(bball)+
  tm_symbols(fill = "brown1", col = "brown1", size = 0.25)

iso_map <- iso_map + 
  tm_title("Driving Distance to Closest Major League Baseball Stadium")

iso_map <- iso_map + 
  tm_credits(text = "Author: Elizabeth C. Delmelle\n@weizman_MUSA\nTravel calculations by MapBox", 
             just = "left", size = 0.5, position = c("left", "bottom"))

iso_map
