library(tidyverse)
library(sf)
library(mapdata)
library(marmap)
library(lubridate)

## AIS data; Downloaded for January 2017, UTM Zone 17
# https://marinecadastre.gov/AIS/ (not included in data folder bc ~1GB)
# subsetted to 2017-1-25 data with script ais_reduce.R in this repo
ais_day = read.csv('data/processed_ais/ais_2017-01-25.csv')

# Coastline data
lat_bounds = c(25, 34)
lon_bounds = c( -82, -76)
world_map = map_data("worldHires", ylim = lat_bounds, xlim = lon_bounds)
dim(world_map)

#Read in US critical habitat shapefiles 
# https://www.greateratlantic.fisheries.noaa.gov/educational_resources/gis/data/index.html
USA_crit_hab = st_read('data/North_Atlantic_Right_Whale_Critical_Habitat/','North_Atlantic_Right_Whale_Critical_Habitat') # reads in set of shapefiles

# Find AIS pings that intersect with RW habitat
Sys.time()
ships_RW_intersect = ais_day %>%
  st_as_sf(coords=c("LON", "LAT"), crs=4269) %>% # AIS uses NAD83 CRS
  st_intersection(USA_crit_hab %>% dplyr::select(geometry))#grabbing only the geometry section 
Sys.time()

head(ships_RW_intersect)

law_breakers = ships_RW_intersect %>% 
  filter(Length > 20, SOG > 10)

dim(law_breakers)
summary(law_breakers)

length(unique(law_breakers$CallSign)) # how many law breakers?
unique(law_breakers$VesselName) # what are their names?
#turn points into lines

#create ship tracks(lines)
illegal_paths = law_breakers %>% 
  mutate(date_time = lubridate::ymd_hms(BaseDateTime)) %>% # date_time is POSIXct class (calendar/time) #changing to a datetime variable that r can read
  arrange(date_time) %>% # ensure ship tracks points are in chronological order
  group_by(CallSign) %>% # essentially groups by call sign but retains length (in meters) info #treating every vessel as a unique path
  summarise(do_union=FALSE) %>% # collapses data into multipoint; do_union=FALSE prevents reordering points
  st_cast("LINESTRING")%>% #gives us a line instead of a multisring
  st_make_valid()

head(illegal_paths)


# plot ship tracks ##group = group keeps the islands together
law_breaking_map = ggplot()+
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black", color = NA) + # add coastline
  geom_sf(data=USA_crit_hab, alpha = 0.5, color=NA, fill='yellow') +
  geom_sf(data=illegal_paths, aes(color=CallSign)) +#adding the law breaking layer# give a different color line to each vessel
  coord_sf(1.3, xlim = lon_bounds, ylim = lat_bounds) + # Crop map edges
  #guides(color=FALSE) + #add a legend
  ylab("Latitude") + xlab("Longitude") + theme_classic() 

law_breaking_map

# What are the lengths of the ship tracks that intersected the RW critical habitat?
illegal_path_lengths = illegal_paths %>%
  st_make_valid() %>% # gets rid of lines with <2 points
  mutate(track_length_m = as.numeric(st_length(geometry)))  # Calculate length of ship tracks in meters
head(illegal_path_lengths)
class(illegal_path_lengths$track_length_m)
typeof(illegal_path_lengths$track_length_m)

sum(illegal_path_lengths$track_length_m)
#676856.6 m / 677km of track length where vessels are breaking the law

tot_illegal_path = illegal_path_lengths %>%
  summarize(tot_track_length_m = sum(track_length_m)) %>%
  mutate(tot_track_length_km = tot_track_length_m/1000) %>%
  st_drop_geometry()

tot_illegal_path

