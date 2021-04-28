library(raster)
library(stars)
library(sf)
library(ncdf4)
library(dplyr)
library(stringr)
library(terra)
library(doParallel)

dir <- "~/University of Exeter/Abrams, Jesse - URBN_CLIMAR/Hack_Feb2021/Hack_Data/UKCP_Data/tasmax/13/"
new_dir <- "~/University of Exeter/Abrams, Jesse - URBN_CLIMAR/Hack_Feb2021/Hack_Data/UKCP_Data/tasmax_bng/13/"

ras_files <- list.files(dir, pattern = "^tas.*.nc$", full.names = TRUE)
#bristl <- st_read("~/Documents/ECEHH/JCEEI_Heat/climr/URBN-CLIMR/data/msoa_shapefiles/bristol_msoas.shp")

if(!dir.exists(new_dir)){
  dir.create(new_dir, recursive = TRUE)
  dir.create("temp_bng_dir")
}

reproj_ras <- function(ras_file){
  #names <- gsub("tasmax_rcp85_land-cpm_uk_2.2km_01_day_", "tasmax", basename(ras_file))
  name <- paste0(new_dir,gsub(".nc", ".tif", basename(ras_file)))
  temp_loc <- paste0("temp_bng_dir/",gsub(".nc", ".tif", basename(ras_file))) # gdal utils doesnt like spaces in dir paths 
  gdal_utils(util = 'warp', source = ras_file, destination = temp_loc, options = c('-tr', '2200',  '2200', '-t_srs', 'EPSG:27700', '-r', 'near'))
  file.rename(temp_loc, name)
 #file.remove(gsub(".tif", ".xml", temp_loc))
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

months <- rep(c(12,1:11), each = 30)
ras_seq <- seq(1,349, by = 30)
msoa_cds <- msoa_vect_all$msoa11cd


for (i in 1:length(tif_files)){
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
   ras_ext <- terra::extract(ras, msoa_fil, touches = TRUE)
   ras_ext <- ras_ext[,-1]
   for(i in ras_seq){
     qs <- quantile(as.vector(t(ras_ext[,i:(i+29)])), probs = c(0, 0.025, 0.05, 0.075, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 0.925, 0.95, 0.975, 1 ))
     means <- mean(as.vector(t(ras_ext[,i:(i+29)])))
     sds <- sd(as.vector(t(ras_ext[,i:(i+29)])))
     df <- data.frame(msoa = msoa,year = years[i],month = months[i], stat = c(0, 0.025, 0.05, 0.075, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 0.925, 0.95, 0.975, 1, "mean", "sd"), value = c(qs, means, sds))
     df_out <- rbind(df_out, df)
   }
  }
  write.csv(df_out, paste0("URBN-CLIMR/data/UKCP/tasmax_quantiles_test/quantiles_",name), row.names = FALSE)
  end <- Sys.time()
  print(end-start)
}


no_cores <- detectCores() - 1  
cl <- makeCluster(no_cores, type="FORK")  
registerDoParallel(cl)  

foreach(i = 1:length(tif_files)) %dopar% {
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
    ras_ext <- terra::extract(ras, msoa_fil, touches = TRUE, weights = TRUE)
    ras_ext <- ras_ext[,-1]
    for(i in ras_seq){
      qs <- quantile(as.vector(t(ras_ext[,i:(i+29)])), probs = c(0, 0.025, 0.05, 0.075, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 0.925, 0.95, 0.975, 1 ))
      means <- mean(as.vector(t(ras_ext[,i:(i+29)])))
      sds <- sd(as.vector(t(ras_ext[,i:(i+29)])))
      df <- data.frame(msoa = msoa,year = years[i],month = months[i], stat = c(0, 0.025, 0.05, 0.075, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 0.925, 0.95, 0.975, 1, "mean", "sd"), value = c(qs, means, sds))
      df_out <- rbind(df_out, df)
    }
  }
  write.csv(df_out, paste0("URBN-CLIMR/data/UKCP/tasmax_quantiles_test/quantiles_",name), row.names = FALSE)
  end <- Sys.time()
  print(end-start)
}

stopCluster(cl)
