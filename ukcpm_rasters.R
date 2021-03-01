library(raster)
library(stars)
library(sf)
library(ncdf4)
library(dplyr)
library(stringr)
library(terra)

ras_files <- list.files("URBN-CLIMR/data/UKCP/tasmax/", pattern = "^tas.*.nc$", full.names = TRUE)
#bristl <- st_read("~/Documents/ECEHH/JCEEI_Heat/climr/URBN-CLIMR/data/msoa_shapefiles/bristol_msoas.shp")

if(!dir.exists("URBN-CLIMR/data/UKCP/tasmax_bng/")){
  dir.create("URBN-CLIMR/data/UKCP/tasmax_bng/")
}

reproj_ras <- function(ras_file){
  #names <- gsub("tasmax_rcp85_land-cpm_uk_2.2km_01_day_", "tasmax", basename(ras_file))
  name <- paste0("URBN-CLIMR/data/UKCP/tasmax_bng/",gsub(".nc", ".tif", basename(ras_file)))
  gdal_utils(util = 'warp', source = ras_file, destination = name, options = c('-tr', '2200',  '2200', '-t_srs', 'EPSG:27700', '-r', 'near'))
  print(basename(ras_file))
  }

bng_ras <- lapply(ras_files, reproj_ras)
#bng_ras <- stack(bng_ras)

tif_files <- list.files("URBN-CLIMR/data/UKCP/tasmax_bng/", pattern = "*.tif$", full.names = TRUE)
#msoas <- st_read("URBN-CLIMR/data/msoa_shapefiles//bristol_msoas.shp")
msoas <- st_read("URBN-CLIMR/data/msoa_shapefiles//Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries.shp")

msoas <- st_transform(msoas, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs")

msoa_sp_all <- as(msoas, "Spatial")
msoa_vect_all <- vect(msoa_sp_all)

quantile_rasters <- function(tif_file){
  
  ras <- brick(tif_file, stopIfNotEqualSpaced = F)
  indices <- rep(1:12, each = 30)
  stackApply(ras, indices = indices, fun = mean)
  
}

months <- rep(c(12,1,2,3,4,5,6,7,8,9,10,11), each = 30)
ras_seq <- seq(1,349, by = 30)
msoa_cds <- msoa_vect_all$msoa11cd



for (i in 2:length(tif_files)){
  start <- Sys.time()
  name <-  gsub(".tif",".csv", basename(tif_files[i]))
  df_out <- NULL
  #ras <- brick(tif_files[i], stopIfNotEqualSpaced = F)
  ras <- rast(tif_files[i])
  print(name)
  years <- c(rep(as.numeric(str_sub(basename(tif_files[i]), -21, -18)), 30), rep(as.numeric(str_sub(basename(tif_files[i]), -12, -9)), 330))
  for (msoa in msoa_cds){
    #msoa_fil <- msoas %>% 
    #  filter(msoa11cd == msoa)
   msoa_fil <- subset(msoa_vect_all, msoa_vect_all$msoa11cd == msoa)
   #ras_ext <- extract(ras, msoa_fil)
   ras_ext <- terra::extract(ras, msoa_fil)
   for(i in ras_seq){
     qs <- quantile((ras_ext[[1]][,i:(i+29)]), probs = c(0, 0.025, 0.05, 0.075, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 0.925, 0.95, 0.975, 1 ))
     means <- mean((ras_ext[[1]][,i:(i+29)]))
     sds <- sd((ras_ext[[1]][,i:(i+29)]))
     df <- data.frame(msoa = msoa,year = years[i],month = months[i], stat = c(0, 0.025, 0.05, 0.075, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 0.925, 0.95, 0.975, 1, "mean", "sd"), value = c(qs, means, sds))
     df_out <- rbind(df_out, df)
   }
   
  }
  write.csv(df_out, paste0("URBN-CLIMR/data/UKCP/tasmax_quantiles/quantiles_",name), row.names = FALSE)
  end <- Sys.time()
  print(end-start)
}


library(doParallel)  
no_cores <- detectCores() - 1  
cl <- makeCluster(no_cores, type="FORK")  
registerDoParallel(cl)  

foreach(i = 1:length(tif_files)) %dopar% {
  start <- Sys.time()
  name <-  gsub(".tif",".csv", basename(tif_files[i]))
  df_out <- NULL
  ras <- brick(tif_files[i], stopIfNotEqualSpaced = F)
  print(name)
  years <- c(rep(as.numeric(str_sub(basename(tif_files[i]), -21, -18)), 30), rep(as.numeric(str_sub(basename(tif_files[i]), -12, -9)), 330))
  for (msoa in msoa_cds){
    msoa_fil <- msoas %>% 
      filter(msoa11cd == msoa)
    ras_ext <- extract(ras, msoa_fil)
    for(i in ras_seq){
      qs <- quantile((ras_ext[[1]][,i:(i+29)]), probs = c(0, 0.025, 0.05, 0.075, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 0.925, 0.95, 0.975, 1 ))
      means <- mean((ras_ext[[1]][,i:(i+29)]))
      sds <- sd((ras_ext[[1]][,i:(i+29)]))
      df <- data.frame(msoa = msoa,year = years[i],month = months[i], stat = c(0, 0.025, 0.05, 0.075, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 0.925, 0.95, 0.975, 1, "mean", "sd"), value = c(qs, means, sds))
      df_out <- rbind(df_out, df)
    }
    
  }
  write.csv(df_out, paste0("URBN-CLIMR/data/UKCP/tasmax_quantiles/quantiles_",name), row.names = FALSE)
  end <- Sys.time()
  print(end-start)
}

foreach(i = 1:length(tif_files)) %dopar% {
  print(tif_files[i])
  }




#write.csv(df_out, "tas_max_quantiles_bristol_msoas.csv", row.names = FALSE)


ggplot(df_out %>% filter(stat == 1), aes(x = year, y = value))+
  geom_point()+
  labs(x = "Year", y = "Max Temp")

ggplot(df_out %>% filter(stat == 0.975), aes(x = year, y = value))+
  geom_jitter(alpha = 0.1)+
  labs(x = "Year", y = "Max Temp - 97.5 quartile")+
  theme_bw()



avg_rasters <- function(tif_file){
  ras <- brick(tif_file, stopIfNotEqualSpaced = F)
  ras_avg <- calc(ras, mean)
  name <- gsub(".tif", "_mean.tif", tif_file)
  writeRaster(ras_avg, name)
  return(avg_rasters)
}

bng_ras_files <- list.files("URBN-CLIMR/data", pattern = "_mean.tif$", full.names = TRUE)

avg_stack <- stack(bng_ras_files)

names(avg_stack) <- basename(bng_ras_files)


time <- 1:nlayers(avg_stack)
X <- cbind(1, time)
invXtX <- solve(t(X) %*% X) %*% t(X)

## much reduced regression model; [2] is to get the slope
quickfun <- function(y) (invXtX %*% y)[2]

x4 <- calc(avg_stack, quickfun)

plot(x4)
buff_meters <- 50000
e <- extent(358337 - buff_meters,358337 + buff_meters,172855 - buff_meters,172855+buff_meters)
  
plot(crop(x4, e))

files <- list.files("URBN-CLIMR/data/UKCP/reprojected_bng/", pattern = "*.tif$", full.names = TRUE)



bristol_crop <- function(file, buffer){
  
  ras <- brick(file, stopIfNotEqualSpaced = F)
  e <- extent(358337 - buffer,358337 + buffer,172855 - buffer,172855+buffer)
  crop_ras <- crop(ras, e)
  name <- paste0("bristol_",basename(file))
  name_dir <- paste0("URBN-CLIMR/data/UKCP/bristol_50km_crop_bng", name)
  writeRaster(crop_ras, name_dir)
  return(crop_ras)
}

cropped_rasters <- lapply(X = files, bristol_crop, buffer = 50000)

files <- list.files(paste0("URBN-CLIMR/data/UKCP/bristol_50km_crop_bng"),full.names = TRUE, pattern = "*.tif$" )

heatwave_days <- function(ras){
  ras_in <- brick(ras)
  hw_days <- calc(ras_in, function(x){sum(x >= 25)})
  names(hw_days) <- basename(ras)
  return(hw_days)
}

hw_days_all <- lapply(X = files, FUN = heatwave_days)


consecutive_hw_days <- function(ras){
  
  ras_in <- brick(ras)
  hw_con_days_4 <- calc(ras_in, function(x){sum(rle(x >= 25)$lengths[rle(x >= 25)$values] >= 4)})
  return(hw_con_days_4) 
}

hw_days_all <- lapply(X = files, FUN = consecutive_hw_days)





ras <- brick("~/Downloads/tas_rcp85_land-cpm_uk_2.2km_01_day_19801201-19811130.nc", stopIfNotEqualSpaced = F)

crs(ras) <- "+a=6371229.0+b=6371229.0+proj=ob_tran+o_proj=latlon+o_lon_p=0.0+o_lat_p=37.5+lon_0=357.5+to_meter=0.0174532925199433+no_defs"


gdal_utils(util = 'warp', source = "tas_rcp85_land-cpm_uk_2.2km_01_day_19801201-19811130.nc", destination = 'tas_2.2_day_1980-1981.tif',
           options = c('-tr', '2200',  '2200', '-t_srs', 'EPSG:27700', '-r', 'near', '-overwrite'))


gdal_utils(util = 'warp', source = "avg_rate_warm.tif", destination = 'os_gb_avg_rate_warm.tif',
           options = c('-tr', '2200',  '2200', '-t_srs', 'EPSG:27700', '-r', 'near', '-overwrite'))


#to rotate the pole to British National Grid – new file written as a .tif file
#when run, use the new .tif file in the extraction code on page 4
gdal_utils(util = 'warp', source = "tas_rcp85_land-cpm_uk_2.2km_01_day_19801201-19811130.nc", destination = 'tas_2.2_day_1980-1981.tif',
           options = c('-tr', '2200',  '2200', '-t_srs', 'EPSG:27700', '-r', 'near', '-overwrite'))

osgb <- st_read("~/Downloads/OSGB_Grids-master/Shapefile/OSGB_Grid_50km.shp")

ras2 <- stack("tas_2.2_day_1980-1981.tif")
plot(osgb)

plot(ras2[[162]])
points(358337,172855)
