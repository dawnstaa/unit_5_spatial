library(sf)
install.packages("sf")
# simple features (spatial vector data) st_read, st_transform
# library(broom) # part of tidyverse
library(tidyverse)
library(mapdata)  # map_data
library(marmap) # getNOAA.bathy()

#Read in US critical habitat shapefiles 
# https://www.greateratlantic.fisheries.noaa.gov/educational_resources/gis/data/index.html
USA_crit_hab = st_read('data/North_Atlantic_Right_Whale_Critical_Habitat/','North_Atlantic_Right_Whale_Critical_Habitat') 

USA_crit_hab
# transform to WGS84 using st_transform from the sf package
USA_crit_hab_sf = st_transform(USA_crit_hab, crs=4326) 
#crs="+proj=longlat +datum=WGS84")

#Reading in the Canadian data
#Load in Canadian RW critical habitat coordinates http://www.dfo-mpo.gc.ca/species-especes/profiles-profils/rightwhaleNA-baleinenoireAN-eng.html
CAN_crit_hab = read.csv('data/NARW_canadian_critical_habitat_2017.csv')
head(CAN_crit_hab)
#list the 1st polygon point again to close the polygon


#Turning csv into an sf (simple feature)
# Turn data frame into sf points, then sf polygon
CAN_crit_hab_sf = CAN_crit_hab %>% 
  st_as_sf(coords=c("lon","lat"), crs=4326) %>% # convert to sf
  dplyr::group_by(habitat, country) %>% #grouping by habitat and country
  dplyr::summarise(do_union=FALSE) %>% # collapses the 8 points in data into multipoint; do_union=FALSE prevents reordering points; check out ?summarise.sf
  st_cast("POLYGON") # converts btwn spatial geometries#converting to a polygon-type data
print(CAN_crit_hab_sf) # 2 simple features, with habitat and country attributes
#find out more via summarise.sf in help
#sometimes after collapsing the points may be reordered

# Simply USA_crit_hab data frame to match CAN_crit_hab
plot(USA_crit_hab_sf$geometry[1], axes=TRUE) # GOM habitat
plot(USA_crit_hab_sf$geometry[2], axes=TRUE) # FL / GA habitat

USA_crit_hab_sf$habitat=c("NMFS_GOM", "SEUS")
USA_crit_hab_sf$country="USA" #two features with one value will be recycled
#the next makes the us data correspond perfectly with the canadian data
USA_crit_hab_sf = USA_crit_hab_sf %>% 
  dplyr::select(country, habitat, geometry) # drops all other variables from shapefile

# Join the USA and Canada critical habitat sf objects
crit_hab = rbind(USA_crit_hab_sf, CAN_crit_hab_sf)

# set GOM + GSL map limits
lon_bounds = c(-72, -54)
lat_bounds = c(39, 53)

# Coastline data
world_map = map_data("worldHires", ylim = lat_bounds,
                     xlim = lon_bounds)

# plot critical habitats and carcass locations
crit_map = ggplot()+ #try to put the raster first and then a coastline
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black") + # add coastline #there's 'group' in th data
  geom_sf(data=crit_hab, alpha = 0.5, aes(fill=country)) + #fill colors based of off country to distinguish country
  geom_point(data = carcass, aes(x = Longitude, y = Latitude, color = Carcass_position), size=2) + 
  coord_sf(1.3, xlim = lon_bounds, ylim = lat_bounds) + # Crop map edges # provides boundaries to the map
  ylab("Latitude") + xlab("Longitude") + theme_classic() 

crit_map #print to screen

# Carcass location data
carcass = read.csv('data/RW_carcasses_2017.csv')#whale carcass data
head(carcass)


#library(ggnewscale)  # new_scale_fill() add to ggplot to create 2nd fill scale
bath_m_raw = marmap::getNOAA.bathy(lon1 = lon_bounds[1]-2, lon2 = lon_bounds[2]+2,
                                   lat1 = lat_bounds[1]-2, lat2 = lat_bounds[2]+2, resolution = 4) # resolution default: 4 minutes
# convert bathymetry to data frame
bath_m_fortify = marmap::fortify.bathy(bath_m_raw) 
bath_m = bath_m_fortify %>%
  mutate(depth_m = ifelse(z>0, NA, z)) %>%
  dplyr::select(-z)
# head(bath_m)
# summary(bath_m)

# plot critical habitats and carcass locations over bathymetry
rw_map = ggplot()+
  geom_raster(data = bath_m , aes(x = x, y = y, fill = depth_m), alpha=0.75) + 
  scale_fill_gradientn(colors=c("black", "navy", "blue4","lightblue"), 
                       values = scales::rescale(c(-5000, -3000, -300, 0)), 
                       name="Depth (m)") +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black", color = NA) +
  geom_sf(data=crit_hab, alpha = 0.5, fill='yellow') +
  geom_point(data = carcass, aes(x = Longitude, y = Latitude, color = Carcass_position), size=2) + 
  coord_sf(1.3, xlim = lon_bounds, ylim = lat_bounds) + # Crop map edges
  ylab("Latitude") + xlab("Longitude") + theme_classic()

rw_map
ggsave(rw_map, filename='figures/RW_habitats.pdf', device="pdf", height=5, width=7)
## How many carcass locations occur in critical habitat zones?

