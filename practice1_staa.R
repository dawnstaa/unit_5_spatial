 
library(tidyverse)
install.packages("raster")
install.packages("rgdal")
library(raster)
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
bath_m_df = marmap::fortify.bathy(bath_m_raw)
head(bath_m_df)

#ifelse(z>20, NA, z) if z>20 let it be equal to NA otherwise make it z
bath_m = bath_m_df%>%
  mutate(depth_m = ifelse(z>20, NA, z)) %>% # 20m gives us wiggle room from sea level for tides/coastline
  dplyr::select(-z)
head(bath_m)
summary(bath_m)
  
# plot raster data
#group=group so that islands are drawn separately, color is the boundary, fill is the actual color
#coord fix helps us to crop b4 imaging to save computing time, expand =false takes away the boundary
#rescale takes a vector
#breaks=c(-100, -300), 
GOM_bath_map = ggplot()+
  geom_raster(data = bath_m , aes(x = x, y = y, fill = depth_m)) + 
  #geom_contour(data = bath_m , aes(x = x, y = y, fill = depth_m), breaks=c(-100), size=0.25) + 
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "darkgrey", color = NA) + # add coastline; group keeps multipolygon coordinates separated into distinct groups
  coord_fixed(1.3, xlim = lon_bounds, ylim = lat_bounds, expand=FALSE) + # Crop map edges
  scale_fill_gradientn(colors=c("black", "darkblue", "lightblue"), 
                       values = scales::rescale(c(-6000, -300, 0)), # rescale to make 2 different gradients (rescale function from scales package in tidyverse)
                       name="Depth (m)") +
  ylab("Lat") + xlab("Lon") + theme_bw() 

GOM_bath_map # print to screen
ggsave(GOM_bath_map, filename='figures/GOM_bath_raster.pdf', device="pdf", height=5, width=7)

#For bathymetry data, it can be nice to add contour lines to the plot. Take a look at these contour maps without and without the underlying colored bathymetry data:
# plot contours
GOM_bath_map_contours = ggplot()+
  geom_contour(data = bath_m, aes(x=x, y=y, z=depth_m), breaks=c(-100), size=c(0.25), colour="grey") + # add 100m contour
  geom_contour(data = bath_m, aes(x=x, y=y, z=depth_m), breaks=c(-200), size=c(0.5), colour="grey") + # add 250m contour
  geom_contour(data = bath_m, aes(x=x, y=y, z=depth_m), breaks=c(-500), size=c(0.75), colour="grey") + # add 250m contour
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black", color = NA) + # add coastline
  coord_fixed(1.3, xlim = lon_bounds, ylim = lat_bounds, expand=FALSE) + # Crop map edges
  ylab("Latitude") + xlab("Longitude") + theme_classic()

GOM_bath_map_contours # print to screen
ggsave(GOM_bath_map_contours, filename='figures/GOM_bath_contours.pdf', device="pdf", height=5, width=7)
ggsave(GOM_bath_map, filename='figures/GOM_bath_raster.pdf', device="pdf", height=5, width=7)


# plot contours
GOM_bath_map_contours = ggplot()+
  geom_contour(data = bath_m, aes(x=x, y=y, z=depth_m), breaks=c(-100), size=c(0.25), colour="grey") + # add 100m contour
  geom_contour(data = bath_m, aes(x=x, y=y, z=depth_m), breaks=c(-200), size=c(0.5), colour="grey") + # add 250m contour
  geom_contour(data = bath_m, aes(x=x, y=y, z=depth_m), breaks=c(-500), size=c(0.75), colour="grey") + # add 250m contour
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black", color = NA) + # add coastline
  coord_fixed(1.3, xlim = lon_bounds, ylim = lat_bounds, expand=FALSE) + # Crop map edges
  ylab("Latitude") + xlab("Longitude") + theme_classic()

GOM_bath_map_contours # print to screen
ggsave(GOM_bath_map_contours, filename='figures/GOM_bath_contours.pdf', device="pdf", height=5, width=7)

# The GOM cropped data we made above:
class(chl_GOM_raster)
class(bath_m_raw)  # "bathy" class (from marmap)

# convert bathymetry to raster
bath_m_raster = marmap::as.raster(bath_m_raw)

# Note the CRS is the same for both rasters WGS84
# Extent is slightly different 
# bath_m resolution is higher than chl resolution
chl_GOM_raster
bath_m_raster

# Rename the bathymetry raster layer so its easier to work with
names(bath_m_raster) = "bath_m"

# resample bath_m to match chl_a
bath_layer_chl_dims = raster::resample(bath_m_raster, chl_GOM_raster) # resamples r1 extent, origin and resolution to that of r2


# If CRS didn't match up, we'd also project the bath_m raster to match the CRS of the chl raster:
# bath_layer_chl_dims_proj = raster::projectRaster(bath_m_raster, crs = crs(chl_GOM_raster))

# now that extent, origin, resolution and projection match, create raster stack
raster_stack = stack(chl_GOM_raster, bath_layer_chl_dims)
raster_stack
plot(raster_stack) # double check everything is oriented correctly

# convert to data frame
stack_df = data.frame( raster::rasterToPoints(raster_stack))
head(stack_df)
summary(stack_df)
dim(stack_df)

# O'Reilly et al. 2019
# chl_a benchmarks for oligo- meso- and eutrophic ocean waters derived from SeaWiFS data
oligo_chl_a = 0.1 # chl_a < 0.1 mg/m^3
eutro_chl_a = 1.67 # chl_a > 1.67 mg/m^3

stack_df = stack_df %>%
  mutate(trophic_index = case_when(chl_a < oligo_chl_a ~ "oligotrophic",
                                   chl_a >= oligo_chl_a & chl_a <= eutro_chl_a ~ "mesotrophic",
                                   chl_a > eutro_chl_a ~ "eutrophic")) %>%
  mutate(trophic_index = as.factor(trophic_index)) #changing trophic index into a factor




