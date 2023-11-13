bball <- st_read("Major_League_Baseball_Stadiums.shp")

bball <- bball %>% 
  mutate(LEAGUE = ifelse(CITY == 'Houston','American', LEAGUE))%>%
  mutate(TEAM = ifelse(TEAM == 'Cleveland Indians', 'Cleveland Guardians', TEAM))

final <- filter(bball, CITY == 'Houston' | CITY == 'Philadelphia' | CITY == 'Arlington'| CITY == 'Phoeniz')

us <- get_decennial(
  geography = "state", 
  year = 2010,
  variables = "P001001",
  geometry = TRUE
)

us <- st_transform(us, crs = 'EPSG:5070')
cont_us <- us%>% filter(NAME != 'Alaska' & NAME != 'Hawaii')

final <- st_transform(final, crs = 'EPSG:5070' )
bball <- st_transform(bball, crs = 'EPSG:5070' )
us <- st_transform(us, crs = 'EPSG:5070' )

wins <- read.csv("wins.csv")
joined <- left_join(bball, wins)


hex <- st_read('us_states_hexgrid.shp')
hex_crs <- st_crs(hex)

joined_wgs <- st_transform(joined, hex_crs)
##summarize number of appearances by state
sum_appear <- joined %>% group_by(STATE)%>%summarize(total_appearances= sum(Appear))%>%st_drop_geometry(sum_appear)
##join to hex
#rename field
hex <- hex %>% rename('STATE' = 'iso3166_2')
hex_joined <- left_join(hex, sum_appear)

hex_joined <- hex_joined%>% replace_na(total_appearances,0)
hex_joined <- hex_joined %>% mutate(total_appearances = ifelse(is.na(total_appearances), 0, total_appearances))
library(tmap)


hex_map <- tm_shape(hex_joined) + 
  tm_polygons(fill = "total_appearances", style = "jenks",n = 6, title = "Total Appearances", legend.is.portrait = FALSE)+
  tm_borders(col = 'white')+
  tm_text(text = "STATE", size = 0.75, col = "black")+
  tm_layout(frame = FALSE, legend.frame = FALSE)+
  tm_title(text = "World Series Appearances by State", size =2)
hex_map


