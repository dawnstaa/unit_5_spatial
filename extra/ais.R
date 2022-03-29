#

library(tidyverse)
library(sf)
library(mapdata)
library(marmap)
library(lubridate)

#########################################################################
#   Read in AIS data and US NARW critical habitats
#########################################################################

## AIS data; Downloaded for January 2017, UTM Zone 17
# https://marinecadastre.gov/AIS/

# Read in AIS data for 2017-1-25
ais_day = read.csv('data/processed_ais/ais_2017-01-25.csv')

# Coastline data
world_map = map_data("worldHires", ylim = lat_bounds, xlim = lon_bounds)

#Read in US critical habitat shapefiles 
# https://www.greateratlantic.fisheries.noaa.gov/educational_resources/gis/data/index.html
USA_crit_hab = st_read('data/North_Atlantic_Right_Whale_Critical_Habitat/','North_Atlantic_Right_Whale_Critical_Habitat') # reads in set of shapefiles
USA_crit_hab
USA_crit_hab_sf = st_transform(USA_crit_hab, crs=4326) #crs="+proj=longlat +datum=WGS84")

#########################################################################
#   Plot AIS points for 2017-1-25
#########################################################################

lat_bounds = c(25, 34)
lon_bounds = c( -82, -76)

head(ais_day)

# plot critical habitats and carcass locations
ais_map_pts = ggplot()+
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black", color = NA) + # add coastline
  geom_point(data = ais_day, aes(x = LON, y = LAT, color = CallSign), size=2) + 
  coord_sf(1.3, xlim = lon_bounds, ylim = lat_bounds) + # Crop map edges
  guides(color=FALSE) +
  ylab("Latitude") + xlab("Longitude") + theme_classic() 

ggsave(ais_map_pts, filename='figures/ais_pts_2017-01-25.pdf', device="pdf", height=5, width=4)

#########################################################################
#   Collapse AIS points into spatial lines (ship tracks)
#        Plot ship tracks for 2017-1-25
#########################################################################

dim(ais_day)
summary(ais_day)

ais_day_sf = ais_day %>% 
  mutate(date_time = lubridate::ymd_hms(BaseDateTime)) %>%
  arrange(date_time) %>% # ensure ship tracks points are in chronological order
  st_as_sf(coords=c("LON", "LAT"), crs=4326) %>% # '+proj=longlat +datum=WGS84'
  group_by(CallSign, Length) %>% # essentially groups by call sign but retains length info
  summarise(do_union=FALSE) %>% # collapses data into multipoint; do_union=FALSE prevents reordering points
  st_cast("LINESTRING") 

class(ais_day_sf)
head(ais_day_sf)
dim(ais_day_sf)  # went from 119,726 points to 283 lines !!

class(ais_day_sf$BaseDateTime)
class(ais_day_sf$date_time) # POSIXct is the date-time class

# plot ship tracks
ais_day_line_map = ggplot()+
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black", color = NA) + # add coastline
  geom_sf(data=USA_crit_hab_sf, alpha = 0.5, color=NA, fill='yellow') +
  geom_sf(data=ais_day_sf, aes(color=CallSign)) +
  coord_sf(1.3, xlim = lon_bounds, ylim = lat_bounds) + # Crop map edges
  guides(color=FALSE) +
  ylab("Latitude") + xlab("Longitude") + theme_classic() 

ggsave(ais_day_line_map, filename='figures/ais_lines_2017-01-25.pdf', device="pdf", height=5, width=4)

#########################################################################
#   Compare 2017-01-25 ship tracks with NARW critical habitats
#########################################################################

# library(lwgeom)
# Find ship tracks that enter RW habitat with spatial join
ships_RW_join = ais_day_sf %>%
  st_make_valid() %>%
  st_join(USA_crit_hab_sf %>% select(geometry), left=FALSE) # spatial inner join (only keep lines that intersect with polygon)

# How many ships went into the RW critical habitat on 2017-1-25?
dim(ships_RW_join)

# What are the lengths of the ships that intersected the RW critical habitat?
ggplot() +
  geom_histogram(data=ships_RW_join, aes(x=Length))

# Plot ship tracks that enter RW habitat
ggplot()+
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black", color = NA) + # add coastline
  geom_sf(data=USA_crit_hab_sf, alpha = 0.5, color=NA, fill='yellow') +
  geom_sf(data=ships_RW_join, aes(color=CallSign)) +
  coord_sf(1.3, xlim = lon_bounds, ylim = lat_bounds) + # Crop map edges
  guides(color=FALSE) +
  ylab("Latitude") + xlab("Longitude") + theme_classic() 

# Now just grab portions of ship tracks that intersect with RW habitat
ships_RW_intersect = ais_day_sf %>%
  st_make_valid() %>%
  st_intersection(USA_crit_hab_sf %>% select(geometry)) %>%
  mutate(track_length_m = as.numeric(st_length(geometry))) # Calculate length of ship tracks in meters

# Plot ship tracks that intersect with RW habitat
ggplot()+
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black", color = NA) + # add coastline
  geom_sf(data=USA_crit_hab_sf, alpha = 0.5, color=NA, fill='yellow') +
  geom_sf(data=ships_in_NARW, aes(color=CallSign)) +
  coord_sf(1.3, xlim = lon_bounds, ylim = lat_bounds) + # Crop map edges
  guides(color=FALSE) +
  ylab("Latitude") + xlab("Longitude") + theme_classic() 

# What are the lengths of the ship tracks that intersected the RW critical habitat?
ships_RW_intersect %>%
  summarize(tot_track_length_m = sum(track_length_m)) %>%
  mutate(tot_track_length_km = tot_track_length_m/1000) %>%
  st_drop_geometry()

#########################################################################
#   Compare all Jan 2017 ship tracks with NARW critical habitats
#########################################################################

# Read in AIS data for all Jan 2017 collapsed into spatial lines (ship tracks)
ais_lines = st_read('data/processed_ais/ais_lines_jan_2017/', 'ais_lines_jan_2017')
head(ais_lines)

### Check intersections with full Jan 2017

ships_in_RW = ais_lines %>%
  st_make_valid() %>%
  st_join(USA_crit_hab_sf %>% select(geometry), left=FALSE) # spatial inner join (only keep lines that intersect with polygon)

# How many ships went into the RW critical habitat on 2017-1-25?
dim(ships_in_RW)

# What are the lengths of the ships that intersected the RW critical habitat?
ggplot() +
  geom_histogram(data=ships_in_RW, aes(x=Length))

# try cropping

ships_RW_intersect = ais_day_sf %>%
  st_make_valid() %>%
  st_intersection(USA_crit_hab_sf)

# Plot ship tracks that intersect with RW habitat
ggplot()+
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black", color = NA) + # add coastline
  geom_sf(data=USA_crit_hab_sf, alpha = 0.5, color=NA, fill='yellow') +
  geom_sf(data=ships_in_NARW, aes(color=CallSign)) +
  coord_sf(1.3, xlim = lon_bounds, ylim = lat_bounds) + # Crop map edges
  guides(color=FALSE) +
  ylab("Latitude") + xlab("Longitude") + theme_classic() 

  raster::intersect()
  st_crop(extent=USA_crit_hab_sf)






