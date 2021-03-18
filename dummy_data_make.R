library(raster)
library(sf)
library(dplyr)
library(ggplot2)
library(googledrive)
library(fst)
library(dplyr)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

msoa <- st_read("URBN-CLIMR/data/msoa_shapefiles//bristol_msoas.shp")
msoas <- as.vector(msoa$msoa11cd)
# x <- readRDS("CLIMR//qs_drive_id.RDS")
# if(!file.exists("01_df_shiny.fst.zip")){
#   drive_download(as_id(x), overwrite = T)
# }
# 
# df <- read_fst(unzip("01_df_shiny.fst.zip")) 
# df_bris <- df[df$msoa %in% msoas,]
# saveRDS(df_bris,"CLIMR/bristl_ukcp.RDS")

hw <- readRDS("CLIMR/bristl_ukcp.RDS") %>% 
  filter(stat == 0.5 & month %in% c("Jun", "Jul", "Aug")) %>% 
  group_by(msoa, year) %>% 
  summarise(median_max = median(value)) 

msoa_df <- msoa %>% 
  as_tibble() %>% 
  left_join(., hw, by = c("msoa11cd" = "msoa")) %>% 
  dplyr::select(msoa11cd, hazard = median_max, year)

# ggplot() +
#   geom_sf(data = msoa, aes(fill = hw_prob))

# bristl <- read.csv("URBN-CLIMR/data/population_data/lad_tus_hse_23.txt")
# bristl_dummy <- bristl %>% 
#   mutate(vulnerability = case_when(age < 75 & underlining == 0 ~ 0,
#                                    age < 75 & underlining == 1 ~ 0.05,
#                                    age >= 75 & underlining == 0 ~ 0.10,
#                                    age >= 75 & underlining == 1 ~ 0.15),
#          exposure = 1) %>% 
#   left_join(., msoa_df, by = c("area" = "msoa11cd")) %>% 
#   mutate(risk = vulnerability*hazard*exposure) %>% 
#   group_by(area) %>% 
#   mutate(population = n())
# 
# bristl_summary <- bristl_dummy %>% 
#   dplyr::select(area,year, risk, vulnerability, hazard, exposure, population) %>% 
#   group_by(area, year) %>% 
#   summarise(risk = mean(risk),
#             vulnerability = mean(vulnerability),
#             hazard = mean(hazard),
#             exposure = mean(exposure),
#             population = unique(population)) 
# msoa_summary <- msoa %>% 
#   dplyr::select(msoa11cd, msoa11nm) %>% 
#   left_join(., bristl_summary, by = c("msoa11cd" = "area"))



bristl <- read.csv("../Gavin/HHR.csv")

bristl_summary <- bristl %>%
  select(msoa11cd, vulnerability = zsovi) %>% 
  left_join(., msoa_df, by = "msoa11cd") %>%
  mutate(hazard_sc = range01(hazard))


msoa_summary <- msoa %>% 
     dplyr::select(msoa11cd, msoa11nm) %>% 
     left_join(., bristl_summary, by = "msoa11cd")

msoa_summary <- st_transform(msoa_summary, "+proj=longlat +datum=WGS84")

saveRDS(msoa_summary, "dummy_data_bristol.RDS")

saveRDS(msoa_summary, "CLIMR/dummy_data_bristol.RDS")


