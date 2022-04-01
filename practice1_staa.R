 
library(tidyverse)
install.packages("raster")
install.packages("rgdal")
library(rgdal)
install.packages("mapdata")
library(mapdata)  # map_data worldHires coastline
install.packages("marmap")
library(marmap)# getNOAA.bathy()
# library(ncdf4)  # also good for reading netCDF files

### Collecting, plotting and manipulating raster data

chl_raster = raster('data/A20021822017212.L3m_MC_CHL_chlor_a_9km.nc') # chl_a in mg/m^3 # raster reads in the largest array by default
# Examine data
class(chl_raster)
chl_raster  # See raster attributes

# rename raster layer
names(chl_raster) = "chl_a" # Easier to type!

# convert to data frame
chl_pts = raster::rasterToPoints(chl_raster, spatial = TRUE) # convert to SpatialPointsDataFrame
class(chl_pts)
chl_df  = data.frame(chl_pts)
head(chl_df)

# See range of data to set good limits for color palette
hist(log10(chl_df$chl_a))# to see what the chlorophyll data looks like
#cols = rainbow(7) #gives in rainbow order
cols = rainbow(7, rev=TRUE)[-1] # reverse rainbow color hex codes, drops the first color (purple); 


# Plot global chl # save and then open data
# geom_raster() plots faster, geom_tile() is slightly more precise
global_chl_map = ggplot() +
  geom_raster(data = chl_df , aes(x = x, y = y, fill = log10(chl_a))) + # chl_a typically viewed on log scale
  ggtitle("Global chl_a July climatology") +
  theme_classic() +
  scale_fill_gradientn(colors = cols, limits=c(-1.5, 0.75), name="log_10(chl_a)")#corresponds to chl-a data
ggsave(global_chl_map, filename="figures/global_chl_map.pdf", height=5, width=9)

global_chl_map # Print to screen
ggsave(global_chl_map, filename='figures/global_chl_July.pdf', device="pdf", height=5, width=9)


# set GOM map limits
lon_bounds = c(-72, -62)
lat_bounds = c(39, 47)

## crop GOM
chl_GOM_raster = raster::crop(chl_raster, extent(c(lon_bounds, lat_bounds))) # ?extent


# Convert GOM raster to points and then to a data frame
chl_GOM_df = data.frame( rasterToPoints(chl_GOM_raster, spatial = TRUE) ) # from raster package
head(chl_GOM_df)
chl_GOM_df = chl_GOM_df %>% dplyr::select(-optional) # drop the optional column

# Grab coastline data from R's worldHires data in the mapdata package:
world_map = map_data("worldHires")
head(world_map)
# Or use GSHHS Shore line maps for higher resolution data:
# https://eriqande.github.io/2014/12/17/compare-map-resolutions.html

GOM_chl_map = ggplot() +
  geom_raster(data = chl_GOM_df , aes(x = x, y = y, fill = log10(chl_a))) + # geom_tile() gives more precise lines
  geom_polygon(aes(x=long, y = lat, group = group), fill = "darkgrey", data=world_map) + # add coastline
  coord_fixed(1.3, xlim = lon_bounds, ylim = lat_bounds, expand=FALSE) + # crop map; 1.3 y/x aspect ratio (may want to use higher at the poles; use 1 near the equator)
  ggtitle("GOM chl_a July climatology") +
  theme_bw() +
  xlab("Longitude") + ylab("Latitude") +
  scale_fill_gradientn(colors = cols, limits=c(-1, 1.75))  # Note I changed color limits from global
ggsave(GOM_chl_map, filename='figures/GOM_chl_map.pdf', device="pdf", height=5, width=9)


# NOAA bathymetry (from marmap package)
# same GOM map limits
lon_bounds = c(-72, -62)
lat_bounds = c(39, 47)

# Download bathymetry data from NOAA (in meters)
# install.packages("rgdal") # may need to reinstall rgdal and raster packages for marmap to work
bath_m_raw = marmap::getNOAA.bathy(lon1 = lon_bounds[1], 
                                   lon2 = lon_bounds[2],
                                   lat1 = lat_bounds[1], 
                                   lat2 = lat_bounds[2], 
                                   resolution = 4) # resolution default: 4 minutes (arc minutes)


class(bath_m_raw)  # "bathy" class (from marmap)
# convert bathymetry to data frame



