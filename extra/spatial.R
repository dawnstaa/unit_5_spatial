## Spatial analysis for MSCI 599

library(marmap)
library(tidyverse)
library(raster)
library(sf)
library(sp)
library(mapdata)  # map_data




####################################################################
#  Modis Aqua chl_a raster and mapping
####################################################################

library(tidyverse)
library(oceanmap)
library(ncdf4)  # opening netCDF files
library(raster)

# https://www.researchgate.net/publication/315494507_oceanmap_Mapping_oceanographic_data/link/58d2987292851cd76d346f65/download


### Download Modis Aqua data from the NASA Ocean Biology Processing Group

### To access data, you need a NASA Earth Data login:

### Ocean Data Access Portal
# https://oceandata.sci.gsfc.nasa.gov/

### Description of NASA Ocean Color products:
# https://oceancolor.gsfc.nasa.gov/atbd/
  
### Data: Modis Aqua -> Mapped -> Monthly Climatology -> 4km -> chlor_a

# Note the file naming convention
# A = Aqua mission
# YYYYJJJYYYYJJJ  year and julian day of start data; year and julian data of end data; Level 3 monthly resolution; 4km spatial resolution
# File naming convention is changing: 
### https://oceancolor.gsfc.nasa.gov/docs/filenaming-convention/

###
# read in MODIS-Aqua data (NetCDF file)
# This file is the monthly climatology of chloropphyl for July (Julian dates 182-212) generated from years 2002 - 2017
chl_raw = nc_open('data/A20021822017212.L3m_MC_CHL_chlor_a_4km.nc') # chl_a in mg/m^3

# convert to raster using oceanmap library
chl_raw_raster = nc2raster(nc=chl_raw, varname="chlor_a", lonname="lon", latname="lat", date=T)

# OBPG data always comes upside down; use oceanmap::flip to flip along y (latitudinal) axis
chl_raster = flip(chl_raw_raster, "y")

# Examine data
class(chl_raster)
chl_raster  # See the raster attributes


# convert to data frame
chl_pts = raster::rasterToPoints(chl_raster, spatial = TRUE) # convert to SpatialPointsDataFrame raster package
chl_df  = data.frame(chl_pts)
# rm(chl_pts) # remove this object from the environment (its big and you dont need it anymore)
head(chl_df)


# See range of data to set good limits for color palette
hist(log10(chl_df$layer))
cols = rainbow(7, rev=TRUE)[-1]
cols = topo.colors(10)


# Plot global chl
ggplot() +
  geom_raster(data = chl_df , aes(x = x, y = y, fill = log10(layer))) + #log10(layer))) + # chl_a is always viewed on the log scale
  ggtitle("Global chl_a July climatology") +
  theme_classic() +
  scale_fill_gradientn(colors = cols, limits=c(-1.5, 0.75), name="log_10(chl_a)") +
  ggsave('figures/global_chl_July.pdf', device="pdf", height=5, width=9)
  # scale_fill_viridis(option="magma", limits=c(-1.5, 0.75))

## Grab GOM
chl_GOM_raster = raster::crop(chl_raster, extent(c(-72, -62, 39, 47))) 

# Convert GOM raster to points and then to a data frame
chl_GOM_df = data.frame( rasterToPoints(chl_GOM_raster, spatial = TRUE) ) %>% # from raster package
  dplyr::select(x, y, chl_a = layer)

# Plot GOM chl

# set GOM map limits
lon_bounds = c(-72, -62)
lat_bounds = c(39, 47)
# Use R's worldHires data:
world_map = map_data("worldHires", ylim = lat_bounds, xlim = lon_bounds)
# Use GSHHS Shore line maps:
# https://eriqande.github.io/2014/12/17/compare-map-resolutions.html

ggplot() +
  geom_raster(data = chl_GOM_df , aes(x = x, y = y, fill = log10(chl_a))) + # chl_a is always viewed on the log scale
  geom_polygon(aes(x=long, y = lat, group = group), fill = "grey80", data=world_map) +
  coord_fixed(ratio=1.3, xlim = lon_bounds, ylim = lat_bounds) +
  ggtitle("GOM chl_a July climatology") +
  theme_bw() +
  xlab("Longitude") + ylab("Latitude") +
  scale_fill_gradientn(colors = cols, limits=c(-1, 1.75)) + # Note I changed color limits from global
  ggsave('figures/GOM_chl_July.pdf', device="pdf", height=5, width=7)

##############################################################
#     NOAA bathymetry (from marmap package)
##############################################################

bath_m_raw = marmap::getNOAA.bathy(lon1 = lon_bounds[1], lon2 = lon_bounds[2],
                             lat1 = lat_bounds[1], lat2 = 47, resolution = 4) # resolution default: 4 minutes
# convert bathymetry to data frame
bath_m_fortify = marmap::fortify.bathy(bath_m_raw) 
bath_m = bath_m_fortify %>%
  mutate(depth_m = ifelse(z>0, NA, z)) %>%
  dplyr::select(-z)
head(bath_m)
summary(bath_m)

# plot raster data
ggplot()+
  geom_raster(data = bath_m , aes(x = x, y = y, fill = depth_m)) + 
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "darkgrey", color = NA) + # add coastline
  coord_fixed(1.3, xlim = lon_bounds, ylim = lat_bounds) + # Crop map edges
  scale_fill_gradientn(colors=c("black", "darkblue", "lightblue"), 
                       values = scales::rescale(c(-6000, -300, 0)), # rescale to make 2 different gradients
                       name="Depth (m)") +
  ylab("Lat") + xlab("Lon") + theme_bw() +
  ggsave('figures/GOM_bath_raster.pdf', device="pdf", height=5, width=7)

# plot contours
ggplot()+
  geom_contour(data = bath_m, aes(x=x, y=y, z=depth_m), breaks=c(-100), size=c(0.25), colour="grey") + # add 100m contour
  geom_contour(data = bath_m, aes(x=x, y=y, z=depth_m), breaks=c(-200), size=c(0.5), colour="grey") + # add 250m contour
  geom_contour(data = bath_m, aes(x=x, y=y, z=depth_m), breaks=c(-500), size=c(0.75), colour="grey") + # add 250m contour
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black", color = NA) + # add coastline
  # geom_polygon(data = ply, aes(x = lon, y = lat), color = "black", alpha = 0.3) + # add poly
  # geom_path(data = lin, aes(x = lon, y = lat), colour = "black", alpha = 1, size=0.3) + #add line
  # geom_point(data = pts, aes(x = lon, y = lat), colour = "black", fill = "grey", stroke = .5, size = 2, alpha = 1, shape = 21) + #add points
  coord_fixed(1.3, xlim = lon_bounds, ylim = lat_bounds) + # Crop map edges
  ylab("Latitude") + xlab("Longitude") + theme_classic() +
  ggsave('figures/GOM_bath_contours.pdf', device="pdf", height=5, width=7)


##############################################################
#     Combine bathymetry and chlorophyll rasters
##############################################################

# convert bathymetry to raster
bath_m_raster = marmap::as.raster(bath_m_raw)

# Name the raser layers in the chl_a and bath_m rasters
names(chl_GOM_raster) = "chl_a"
names(bath_m_raster) = "bath_m"

# Note the CRS is the same for both rasters WGS84
# Extent is slightly different 
# chl_a resolution is higher than bath_m resolution
chl_GOM_raster
bath_m_raster

# resample bath_m to match chl_a
bath_layer_chl_dims = raster::resample(bath_m_raster, chl_GOM_raster) # resamples r1 extent, origin and resolution to that of r2

# now that extent, origin, resolution and projection match, create raster stack
raster_stack = stack(chl_GOM_raster, bath_layer_chl_dims)
plot(raster_stack) # double check everything is oriented correctly

# convert to data frame
stack_df = data.frame( raster::rasterToPoints(raster_stack))
head(stack_df)

range(stack_df$chl_a, na.rm=TRUE)
summary(stack_df)

dim(stack_df)

# O'Reilly et al. 2019
# chl_a benchmarks for oligo- meso- and eutrophic ocean waters derived from SeaWiFS data
oligo_chl_a = 0.1 # mg/m^3
eutro_chl_a = 1.67 # mg/m^3

stack_df = stack_df %>%
  mutate(trophic_index = case_when(chl_a < oligo_chl_a ~ "oligotrophic",
                                   chl_a > oligo_chl_a & chl_a < eutro_chl_a ~ "mesotrophic",
                                   chl_a > eutro_chl_a ~ "eutrophic")) %>%
  mutate(trophic_index = as.factor(trophic_index))

# What portion of our area of interest is clssified as oligotrophic, mesotrophic and eutrophic?
table(stack_df$trophic_index)  # no oligotrophic waters in GOM region
n_eutro = dim(stack_df %>% filter(trophic_index == "eutrophic"))[1]
n_meso = dim(stack_df %>% filter(trophic_index == "mesotrophic"))[1]
pct_eutro = n_eutro / (n_eutro + n_meso)
pct_meso = n_meso / (n_eutro + n_meso)

# Plot histogram of bathymetric depth (m) for each trophic index
ggplot() +
  geom_histogram(aes(x=bath_m), data=stack_df) +
  facet_wrap(~trophic_index)






##############################################################
#        Right Whale 2017 UME Map
##############################################################


##############################################################
#     NOAA bathymetry for GOM + GSL (from marmap package)
##############################################################

library(sf) # simple features pckg st_read, st_transform

library(broom) #tidy
library(tidyverse)
library(mapdata)  # map_data


# set GOM + GSL map limits
lon_bounds = c(-72, -54)
lat_bounds = c(39, 53)
bath_m_raw = marmap::getNOAA.bathy(lon1 = lon_bounds[1], lon2 = lon_bounds[2],
                                   lat1 = lat_bounds[1], lat2 = lat_bounds[2], resolution = 4) # resolution default: 4 minutes
# convert bathymetry to data frame
bath_m_fortify = marmap::fortify.bathy(bath_m_raw) 
bath_m = bath_m_fortify %>%
  mutate(depth_m = ifelse(z>0, NA, z)) %>%
  dplyr::select(-z)
head(bath_m)
summary(bath_m)

world_map = map_data("worldHires", ylim = lat_bounds, xlim = lon_bounds)

# plot bathymetry 
ggplot()+
  geom_raster(data = bath_m , aes(x = x, y = y, fill = depth_m)) + 
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "darkgrey", color = NA) + # add coastline
  coord_fixed(1.3, xlim = lon_bounds, ylim = lat_bounds) + # Crop map edges
  scale_fill_gradientn(colors=c("black", "navy", "blue4","lightblue"), 
                       values = scales::rescale(c(-5000, -3000, -300, 0)), # rescale to make 2 different gradients
                       name="Depth (m)") +
  ylab("Lat") + xlab("Lon") + theme_bw() 


##############################################################
#     RW Critical Habitat boundaries and 2017 carcass locations
##############################################################

# Carcass location data
carcass = read.csv('data/RW_carcasses_2017.csv')

#Load in Canadian RW critical habitat coordinates http://www.dfo-mpo.gc.ca/species-especes/profiles-profils/rightwhaleNA-baleinenoireAN-eng.html
CAN_crit_hab = read.csv('data/NARW_canadian_critical_habitat_2017.csv')
head(CAN_crit_hab)

# Turn data frame into sf points, then sf polygon
CAN_crit_hab_sf = CAN_crit_hab %>% 
  st_as_sf(coords=c("lon","lat"), crs='+proj=longlat +datum=WGS84') %>%
  dplyr::group_by(habitat, country) %>% 
  dplyr::summarise(do_union=FALSE) %>% # collapses data into multipoint; do_union=FALSE prevents reordering points
  st_cast("POLYGON") 

#Read in US critical habitat shapefiles 
# https://www.greateratlantic.fisheries.noaa.gov/educational_resources/gis/data/index.html

USA_crit_hab = st_read('data/North_Atlantic_Right_Whale_Critical_Habitat/','North_Atlantic_Right_Whale_Critical_Habitat')
USA_crit_hab_trans = st_transform(USA_crit_hab, crs="+proj=longlat +datum=WGS84")
USA_crit_hab_sf = fortify(USA_crit_hab_trans)
USA_crit_hab_sf  # 2 simple features

# Simply USA_crit_hab data frame to match CAN_crit_hab
plot(USA_crit_hab_sf$geometry[1], axes=TRUE) # GOM habitat
plot(USA_crit_hab_sf$geometry[2], axes=TRUE) # FL / GA habitat
USA_crit_hab_sf$habitat=c("NMFS_GOM", "SEUS")
USA_crit_hab_sf$country="USA"
USA_crit_hab_sf = USA_crit_hab_sf %>% select(country, habitat, geometry) # drops all other variables from shapefile

# Join the USA and Canada critical habitat sf objects
crit_hab = rbind(USA_crit_hab_sf, CAN_crit_hab_sf)

# plot critical habitats and carcass locations
ggplot()+
  geom_raster(data = bath_m , aes(x = x, y = y, fill = depth_m), alpha=0.75) + 
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black", color = NA) + # add coastline
  # geom_polygon(data = CAN_crit_hab, aes(x = longitude, y = latitude, fill = critical_habitat), alpha = 0.3) + # add poly
  geom_sf(data=crit_hab, alpha = 0.5, color=NA, fill='yellow') +
  geom_point(data = carcass, aes(x = Longitude, y = Latitude, color = Carcass_position), size=2) + 
  coord_sf(1.3, xlim = lon_bounds, ylim = lat_bounds) + # Crop map edges
  scale_fill_gradientn(colors=c("black", "navy", "blue4","lightblue"), 
                       values = scales::rescale(c(-5000, -3000, -300, 0)), # rescale to make 2 different gradients
                       name="Depth (m)") +
  ylab("Latitude") + xlab("Longitude") + theme_classic() +
  ggsave('figures/RW_habitats.pdf', device="pdf", height=5, width=7)

## How many carcass locations occur in critical habitat zones?

