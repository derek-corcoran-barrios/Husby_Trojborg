library(sf)
library(raster)
library(tidyverse)

## generate a list of the shapefiles for cropping the rasters

Husby <- readRDS("Husby.rds") %>% st_transform(crs= "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")

Trojborg <-readRDS("Trojborg.rds") %>% st_transform(crs= "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")

Shapes <- list(Husby, Trojborg)

# crop the lidar rasters

## TWI

Wetness_lidar_files <- list.files(path = "O:/Nat_Ecoinformatics-tmp/au634851/dk_lidar_backup_2021-03-09/outputs/twi/", full.names = T, pattern = ".vrt")
Wetness_lidar_files <- raster(Wetness_lidar_files)


Twis <- Shapes %>% 
  purrr::map(~crop(Wetness_lidar_files, .x)) %>% 
  purrr::map2(Shapes,~mask(.x, .y))

## Canopy height

Canopy_Cover <- list.files(path = "O:/Nat_Ecoinformatics-tmp/au634851/dk_lidar_backup_2021-03-09/outputs/canopy_height/", full.names = T, pattern = ".vrt")
Canopy_Cover <- raster(Canopy_Cover)


Canopy_Heights <- Shapes %>% 
  purrr::map(~crop(Canopy_Cover, .x)) %>% 
  purrr::map2(Shapes,~mask(.x, .y))

## Vegetation density

Vegetation_dens <- list.files(path = "O:/Nat_Ecoinformatics-tmp/au634851/dk_lidar_backup_2021-03-09/outputs/proportions/vegetation_density/", full.names = T, pattern = ".vrt")
Vegetation_dens <- raster(Vegetation_dens)

VegDens <- Shapes %>% 
  purrr::map(~crop(Vegetation_dens, .x)) %>% 
  purrr::map2(Shapes,~mask(.x, .y))


## Distance to the coast
#shapefile of denmark

Denmark <- getData(name = "GADM", country = "DNK", level = 0) %>% 
  st_as_sf()

Denmark <- st_transform(Denmark, crs = "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")


grid <- st_make_grid(iceland, cellsize = 10, what = "centers")


##Stacks

Husby <- stack(Twis[[1]], Canopy_Heights[[1]], VegDens[[1]]) %>% readAll()

saveRDS(Husby, "HusbyRaster.rds")

Trojborg <- stack(Twis[[2]], Canopy_Heights[[2]], VegDens[[2]])  %>% readAll()

saveRDS(Trojborg, "TrojborgRaster.rds")

