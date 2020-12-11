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
msoa <- st_read(here("URBN-CLIMR/data/bristol_msoas/bristol_msoas.shp"))
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
bris_geo$green_count_n <- bris_geo$green_count/max(bris_geo$green_count)

# Temperature
bris_geo <- tas %>%
  select(msoa11cd, tas) %>%
  left_join(bris_geo, ., by='msoa11cd')
# Normalise tas
bris_geo$tas_n = (bris_geo$tas - 25)/(35 - 25)
bris_geo$tas_n[is.na(bris_geo$tas_n)] = mean(bris_geo$tas_n, na.rm=TRUE)

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
  geom_sf(aes(fill = mean_age_n, colour = mean_age_n))+
  scale_color_continuous_sequential(palette = "Blues", rev=TRUE, 
                                    name="Average age (years)")+
  scale_fill_continuous_sequential(palette = "Blues", rev=TRUE, 
                                   name="Average age (years)")+
  theme_dark() 

# Greenspace
ggplot() + 
  geom_sf(data = bris_geo, aes(fill = green_count_n, color=green_count_n)) +
  scale_color_continuous_sequential(palette = "Greens", rev=FALSE, name="Number of green spaces")+
  scale_fill_continuous_sequential(palette = "Greens", rev=FALSE, name="Number of green spaces") +
  theme_dark() +
  theme(legend.position="none")

# Temperature
ggplot() + 
  geom_sf(data = bris_geo, aes(fill = tas_n, color=tas_n)) +
  scale_color_continuous_sequential(palette = "Reds", rev=FALSE, name="Temperature")+
  scale_fill_continuous_sequential(palette = "Reds", rev=FALSE, name="Temperature") +
  theme_dark() +
  theme(legend.position="none")
