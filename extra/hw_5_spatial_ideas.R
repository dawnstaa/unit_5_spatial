# spatial HW 5 ideas

library(tidyverse)
library(sf)
library(mapdata)
library(marmap)

#########################################################################
#   Plot the boundaries of Gray's Reef NMS (Georgia) with bathymetry
#########################################################################

#Read in Gray's Reef shapefiles 
# https://sanctuaries.noaa.gov/library/imast_gis.html
gray = st_read('data/grays_reef_nms/','grnms_py') # reads in set of shapefiles
plot(gray)

# set GOM + GSL map limits
lon_bounds = c(-82, -80)
lat_bounds = c(30, 32)

# Coastline data
world_map = map_data("worldHires", ylim = lat_bounds, xlim = lon_bounds)

bath_m_raw = marmap::getNOAA.bathy(lon1 = lon_bounds[1], lon2 = lon_bounds[2],
                                   lat1 = lat_bounds[1], lat2 = lat_bounds[2],
                                   resolution = 4) # resolution default: 4 minutes
# convert bathymetry to data frame
bath_m_fortify = marmap::fortify.bathy(bath_m_raw) 
bath_m = bath_m_fortify %>%
  mutate(depth_m = ifelse(z>0, NA, z)) %>%
  dplyr::select(-z)
# head(bath_m)
# summary(bath_m)

# plot critical habitats and carcass locations
ggplot()+
  geom_raster(data = bath_m , aes(x = x, y = y, fill = depth_m), alpha=0.75) + 
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black") + # add coastline
  geom_sf(data=gray, alpha = 0.5) +
  #geom_point(data = carcass, aes(x = Longitude, y = Latitude, color = Carcass_position), size=2) + 
  coord_sf(1.3, xlim = lon_bounds, ylim = lat_bounds) + # Crop map edges
  ylab("Latitude") + xlab("Longitude") + theme_classic() 

#########################################################################
#   Plot RW Seasonal Mgmt Areas
#########################################################################

# set GOM + GSL map limits
lon_bounds = c(-83, -65)
lat_bounds = c(25, 45)

# Coastline data
world_map = map_data("worldHires", ylim = lat_bounds, xlim = lon_bounds)

# https://marinecadastre.gov/data/
rw_sma = st_read('data/shapefile-right-whale-sma-all/','right_whale_SMA_all_ln') # reads in set of shapefiles
plot(rw_sma)

# plot critical habitats and carcass locations
rw = ggplot()+
  #geom_raster(data = bath_m , aes(x = x, y = y, fill = depth_m), alpha=0.75) + 
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black") + # add coastline
  geom_sf(data=rw_sma, aes(color=Restr_Area)) +
  coord_sf(1.3, xlim = lon_bounds, ylim = lat_bounds) + # Crop map edges
  ylab("Latitude") + xlab("Longitude") + theme_classic() 

rw
ggsave(rw, filename="figures/rw.pdf", device="pdf")







