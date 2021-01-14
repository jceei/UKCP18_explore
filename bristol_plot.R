# Courtesy of Dr. Fiona Spooner, University of Exeter

library(sf)
# library(vroom)
library(ggplot2)
library(dplyr)
library(here)
library(readr)
library(colorspace)

# Load data
bristl <- read.csv(here("URBN-CLIMR/data/population_data/lad_tus_hse_23.txt"))
msoa <- st_read(here("URBN-CLIMR/data/msoa_shapefiles//bristol_msoas.shp"))
rivers <- st_read(here("URBN-CLIMR/data/os_open_rivers/data/WatercourseLink.shp"))
green <- st_read(here("URBN-CLIMR/data/os_open_green_space/data/GB_AccessPoint.shp"))
tas <- read.csv(here("URBN-CLIMR/data/UKCP/bristol_tasmax_20210630.csv")) %>%
  rename(tas=air_temperature_mean..degC.)


# Demographic data wrangle
bris_sum <- bristl %>% 
  group_by(area) %>% 
  summarise(mean_wage = mean(medianhourlypay, na.rm = TRUE), mean_age = mean(age))

# Mean age
bris_geo <- msoa %>% 
  filter(msoa11cd %in% bris_sum$area) %>% 
  left_join(., bris_sum, by =c("msoa11cd" = "area", "mean_wage", "mean_age"))
# "Normalised"
bris_geo$mean_age_n = (bris_geo$mean_age - 20)/max(bris_geo$mean_age - 20)

# Rivers
rivers_bris <- rivers %>% 
  st_crop(st_bbox(bris_geo))

# Greenspace
green_bris <- green %>%
  st_crop(st_bbox(bris_geo))
# "Normalised"
bris_geo$green_count <- lengths(st_intersects(bris_geo, green_bris))
bris_geo$green_count_n <- 1-(bris_geo$green_count/max(bris_geo$green_count))

# Temperature
bris_geo <- tas %>%
  select(msoa11cd, tas) %>%
  left_join(bris_geo, ., by='msoa11cd')
# Normalise tas
bris_geo$tas_n = (bris_geo$tas - 20)/(35 - 20)
bris_geo$tas_n[is.na(bris_geo$tas_n)] = mean(bris_geo$tas_n, na.rm=TRUE)

# Impact
bris_geo$impact = bris_geo$tas_n * bris_geo$green_count_n * bris_geo$mean_age_n

# Plots:
# Rivers
ggplot(rivers_bris)+
  geom_sf()

# Mean wage
ggplot() +
  geom_sf(data = bris_geo,aes(fill = mean_wage, color = mean_wage))+
  scale_color_continuous_sequential(palette = "Greens", rev=FALSE, 
                                    name="Average Wage (£/hr)")+
  scale_fill_continuous_sequential(palette = "Greens", rev=FALSE, 
                                   name="Average Wage (£/hr)")+
  # geom_sf(data = rivers_bris, colour = "blue")+
  theme_dark()

# Mean age (Normalised)
bris_geo %>% 
  ggplot(data = .) +
  geom_sf(aes(fill = mean_age_n, colour = mean_age_n)) +
  scale_color_continuous_sequential(palette = "Blues", rev=TRUE, name="Age", breaks=c(0,1),labels=c("Younger","Older"), limits=c(0,1))+
  scale_fill_continuous_sequential(palette = "Blues", rev=TRUE, name="Age", breaks=c(0,1),labels=c("Younger","Older"), limits=c(0,1))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10), color=guide_colourbar(barwidth = 1, barheight = 10)) +
  theme_dark(base_size = 18) 

# Mean age
bris_geo %>% 
  ggplot(data = .) +
  geom_sf(aes(fill = mean_age, colour = mean_age)) +
  scale_color_continuous_sequential(palette = "Blues", rev=TRUE, name="Mean age (years)")+
  scale_fill_continuous_sequential(palette = "Blues", rev=TRUE, name="Mean age (years)")+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10), color=guide_colourbar(barwidth = 1, barheight = 10)) +
  theme_dark(base_size = 18) 


# Greenspace_n
ggplot() + 
  geom_sf(data = bris_geo, aes(fill = green_count_n, color=green_count_n)) +
  scale_color_continuous_sequential(palette = "Greens", rev=TRUE, name="Greenspace", breaks=c(0,1),labels=c("More access","Less access"), limits=c(0,1))+
  scale_fill_continuous_sequential(palette = "Greens", rev=TRUE, name="Greenspace", breaks=c(0,1),labels=c("More access","Less access"), limits=c(0,1)) +
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10), color=guide_colourbar(barwidth = 1, barheight = 10)) +
  theme_dark(base_size = 18) 
  # theme(legend.position="none")
# labels=c('Less access', 'More access'), breaks=c(0,1)

# Greenspace
ggplot() + 
  geom_sf(data = bris_geo, aes(fill = green_count, color=green_count)) +
  scale_color_continuous_sequential(palette = "Greens", rev=TRUE, name="Number of greenspaces")+
  scale_fill_continuous_sequential(palette = "Greens", rev=TRUE, name="Number of greenspaces") +
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10), color=guide_colourbar(barwidth = 1, barheight = 10)) +
  theme_dark(base_size = 18) 

# Temperature
ggplot() + 
  geom_sf(data = bris_geo, aes(fill = tas_n, color=tas_n)) +
  scale_color_continuous_sequential(palette = "Reds", rev=TRUE, name=paste("Temperature (30 Jun 2021)", sep='\n'), breaks=c(0,0.33,0.67,1),labels=c("20°C","25°C","30°C","35°C"), limits=c(0,1))+
  scale_fill_continuous_sequential(palette = "Reds", rev=TRUE, name=paste("Temperature (30 Jun 2021)", sep='\n'), breaks=c(0,0.33,0.67,1),labels=c("20°C","25°C","30°C","35°C"), limits=c(0,1))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10), color=guide_colourbar(barwidth = 1, barheight = 10)) +
  theme_dark(base_size = 18) 
  # theme(legend.position="none")


# "Hazard"
ggplot() + 
  geom_sf(data = bris_geo, aes(fill = tas_n, color=tas_n)) +
  scale_color_continuous_sequential(palette = "Reds", rev=TRUE, name="Hazard", breaks=c(0.5,0.8),labels=c("Low", "High"), limits=c(0.5,0.8))+
  scale_fill_continuous_sequential(palette = "Reds", rev=TRUE, name="Hazard", breaks=c(0.5,0.8),labels=c("Low", "High"), limits=c(0.5, 0.8))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10), color=guide_colourbar(barwidth = 1, barheight = 10)) +
  theme_dark(base_size = 18) 
# theme(legend.position="none")

# Impact
ggplot() + 
  geom_sf(data = bris_geo, aes(fill = impact, color=impact)) +
  scale_color_continuous_sequential(palette = "Grays", rev=TRUE, name="Impact", breaks=c(0,1),labels=c("Lowest","Highest"), limits=c(0,1))+
  scale_fill_continuous_sequential(palette = "Grays", rev=TRUE, name="Impact", breaks=c(0,1),labels=c("Lowest","Highest"), limits=c(0,1))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10), color=guide_colourbar(barwidth = 1, barheight = 10)) +
  theme_dark(base_size = 18) 


bris_geo_84 <- st_transform(bris_geo, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

save(bris_geo_84, file = "climr_bris_data_84.RDA")

#### Reproject Data ###
# # this code below is super messy and far from elegant, was just trying to get things to work...
# # almost workign but not quite
# # I am just picking one time slot, i guess we should average over the year
# # or pick out the max temp or do some other things...number of days over X degrees or
# # use it to compute wet bulb temp with humidity and then pick number of days over X degrees
# library(raster)
# library(ncdf4)

# tmax_nc <- nc_open("~/OneDrive - University of Exeter/URBN_CLIMR/data/UKCP/tasmax_rcp85_land-cpm_uk_2.2km_01_day_19801201-19811130.nc")
# lon <- ncvar_get(tmax_nc,"longitude")
# nlon <- dim(lon)
# lat <- ncvar_get(tmax_nc,"latitude")
# nlat <- dim(lat)
# t_max <- ncvar_get(tmax_nc,"tasmax")
# time <- ncvar_get(tmax_nc,"yyyymmdd")

# tmp <- t_max[,,1]

# library(reshape2)

# tmp1 <- melt(lon)
# tmp2 <- melt(lat)
# tmp3 <- melt(tmp)

# df <- data.frame(lat=tmp2$value,
#                  lon=tmp1$value,
#                  temp=tmp3$value)

# projcrs <- st_crs(bris_geo)
# temp_df <- st_as_sf(x = df,                         
#                     coords = c("lon", "lat"),
#                     crs = "+proj=longlat +datum=WGS84 +no_defs")

# trans_temp <- st_transform(temp_df, projcrs)

# #up to here works. I have the temperature as an sf and on the same projection as bris_geo
# # now its a matter of extracting the average value for each MSOA which I havent gotten to work yet

# # my idea was to convert the dataframe to a raster and then extract the average value from that raster
# # but i am having trouble getting that to work. doesnt want to play nice and convert to a raster
# test <- raster(crs = crs(trans_temp), vals = 0, resolution = c(0.5, 0.5), ext = extent(bris_geo)) %>%
#   rasterize(trans_temp, ., field="temp")

# spg <- df
# coordinates(spg) <- ~ lon + lat
# proj4string(spg)=CRS("+proj=longlat +datum=WGS84 +no_defs") # set it to lat-long
# # coerce to SpatialPixelsDataFrame
# gridded(spg) <- TRUE
# # coerce to raster
# rasterDF <- raster(spg)
# projection(rasterDF) <- "+proj=longlat +datum=WGS84 +no_defs"
# values(rasterDF) <- df$temp

# temp_bris <- trans_temp %>%
#   st_crop(st_bbox(bris_geo))

# bris_geo$temp <- st_intersects(bris_geo, temp_bris)

# ggplot() + 
#   geom_sf(data = bris_geo, aes(fill = temp, color=temp))

# # Create centroid from polygons
# pt <- st_centroid(bris_geo, of_largest_polygon = TRUE)

# test <- st_join(pt,temp_bris)




#test <- rasterize(trans_temp,r.raster,field="temp")
#test2 <- (test,projection(bris_geo))
#extract(test, bris_geo)