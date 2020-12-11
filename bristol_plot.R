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
msoa <- st_read(here("URBN-CLIMR/data/msoa_shapefiles/bristol_msoas.shp"))
rivers <- st_read(here("URBN-CLIMR/data/os_open_rivers/data/WatercourseLink.shp"))
green <- st_read(here("URBN-CLIMR/data/os_open_green_space/data/GB_AccessPoint.shp"))

# 
bris_sum <- bristl %>% 
  group_by(area) %>% 
  summarise(mean_wage = mean(medianhourlypay, na.rm = TRUE), mean_age = mean(age))

bris_geo <- msoa %>% 
  filter(msoa11cd %in% bris_sum$area) %>% 
  left_join(., bris_sum, by =c("msoa11cd" = "area", "mean_wage", "mean_age"))

# "Normalised" mean age
bris_geo$mean_age_n = (bris_geo$mean_age - 20)/max(bris_geo$mean_age - 20)

# Rivers and greenspace data wrangle
rivers_bris <- rivers %>% 
  st_crop(st_bbox(bris_geo))

green_bris <- green %>%
  st_crop(st_bbox(bris_geo))

bris_geo$green_count <- lengths(st_intersects(bris_geo, green_bris))
# Normalised greenspace
bris_geo$green_count_n <- bris_geo$green_count/max(bris_geo$green_count)

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

# Mean age
bris_geo %>% 
  ggplot(data = .) +
  geom_sf(aes(fill = mean_age_n, colour = mean_age_n))+
  scale_color_continuous_sequential(palette = "Blues", rev=TRUE, 
                                    name="Average age (years)")+
  scale_fill_continuous_sequential(palette = "Blues", rev=TRUE, 
                                   name="Average age (years)")+
  theme_dark() 

ggplot() + 
  geom_sf(data = bris_geo, aes(fill = green_count_n, color=green_count_n)) +
  scale_color_continuous_sequential(palette = "Greens", rev=FALSE, name="Number of green spaces")+
  scale_fill_continuous_sequential(palette = "Greens", rev=FALSE, name="Number of green spaces") +
  theme_dark() +
  theme(legend.position="none")

# geom_sf(data = green_bris, aes(color = accessType))
