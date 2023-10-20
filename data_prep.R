##########################################
####  Process data
##########################################
#### | Project name: Amazon fires
#### | Script type: Data processing and Geospatial Analysis
#### | What it does: 
#### | - Loads current and historical fire data (from VIIRS and MODIS sources) for South America.
#### | - Processes road data and intact forest data.
#### | - Generates raster layers from the Intact Forest Landscapes (IFL) and data from Natural Earth.
#### | - Processes indigenous territories data for South America.
#### | - All processed datasets are saved in GeoPackage or TIFF format.
#### | 
#### | Date created: August 26, 2019.
#### | Creator: Mirza Cengic
#### | Contact: mirzaceng@gmail.com
##########################################



# Load packages -----------------------------------------------------------

library(Rahat)
library(tidyverse)
library(lubridate)
library(sf)
library(raster)
library(mapview)
library(janitor)
library(tictoc)
library(fasterize)
library(glue)
library(here)


# Process data ------------------------------------------------------------

# Load recent fires data from VIIRS satellite
viirs_current_name <- "Data/VIIRS_current.gpkg"
# Check if VIIRS current fires dataset exists; if not, process and save
# Process only data with high confidence
if (!file.exists(viirs_current_name)) {
  fires_viirs <- "Data_raw/VNP14IMGTDL_NRT_South_America_7d/VNP14IMGTDL_NRT_South_America_7d.shp" %>%
    st_read()
  
  fires_viirs %>% 
    filter(CONFIDENCE == "high") %>% 
    st_write(viirs_current_name)
}

# Load recent fires data from MODIS
modis_current_name <- "Data/Modis_current.gpkg"
# Check if MODIS current fires dataset exists; if not, process and save
# Process only data with high confidence

if (!file.exists(modis_current_name)) {
  fires_modis <- "Data_raw/MODIS_C6_South_America_7d/MODIS_C6_South_America_7d.shp" %>% 
    st_read()
  fires_modis %>% 
    filter(CONFIDENCE >= 95) %>% 
    st_write(modis_current_name)
}

# Load historical fire data
modis_historical_name <- "Data/Modis_historical.gpkg"
# Check if MODIS historical fires dataset exists; if not, process and save
if (!file.exists(modis_historical_name)) {
  fires_historical_modis <- "Data_raw/DL_FIRE_M6_67354/fire_archive_M6_67354.csv" %>%
    read_csv()
  fires_archive_modis_sf <- fires_historical_modis %>% 
    filter(confidence >= 95) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  st_write(fires_archive_modis_sf, modis_historical_name)
}

viirs_historical_name <- "Data/VIIRS_historical.gpkg"
# Check if VIIRS historical fires dataset exists; if not, process and save
if (!file.exists(viirs_historical_name)) {
  fires_historical_viirs <- "Data_raw/DL_FIRE_V1_67551/fire_archive_V1_67551.csv" %>% 
    read_csv()
  fires_archive_viirs_sf <- fires_historical_viirs %>% 
    filter(confidence == "h") %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  st_write(fires_archive_viirs_sf, viirs_historical_name)
}

# Load and process roads data (Using Global Roads Inventory Project)
# This is to see whether proximity to roads is important driver for fires
roads_filename <- "Projects/Amazon/Data_raw/roads_samer.gpkg"
# Check if roads dataset exists; if not, read and save
if (!file.exists(roads_filename)) {
  roads_raw <- "Projects/Amazon/Data_raw/GRIP4_Region2_vector_fgdb/GRIP4_region2.gdb" %>%
    st_read()
  
  st_write(roads_raw, roads_filename)
}

# Load and process Intact Forest Landscapes (IFL) data
# Check if fires occur in most vulnerable forest areas
ifl_raster_name <- "Data/IFL_raster.tif"
# Check if IFL dataset exists; if not, read, process and save as raster
if (!file.exists(ifl_raster_name)) {
  ifl_data <- st_read("Data_raw/IFL_2016/ifl_2016.shp")
  
  ifl_samer <- ifl_data %>% 
    filter(str_detect(IFL_ID, "SAM"))
  
  lc_1km <- "Projects/Other/Amazon" %>% 
    milkunize2() %>% 
    list.files(pattern = "1km.tif", full.names = TRUE) %>% 
    raster()
  
  ifl_raster <- fasterize(ifl_samer, lc_1km)
  
  names(ifl_raster) <- "IFL_raster"
  writeRaster(ifl_raster, ifl_raster_name, overwrite = TRUE)
}

# Load administrative boundaries and other geographic datasets
brazil_adm_name <- "Data/Brazil_GADM.gpkg"
# Check if Brazil administrative dataset exists; if not, read and save
if (!file.exists(brazil_adm_name)) {
  "Data_raw/gadm36_BRA_1_sf.rds" %>% 
    read_rds() %>% 
    st_write(brazil_adm_name)
}

brazil_bound_name <- "Data/Brazil_GADM_country.gpkg"
# Check if Brazil country boundary dataset exists; if not, process and save
if (!file.exists(brazil_bound_name)) {
  brazilb <- "Data_raw/gadm36_BRA_1_sf.rds" %>% 
    read_rds()
  brazil_bound <- brazilb %>% 
    group_by(NAME_0) %>% 
    summarize()
  st_write(brazil_bound, brazil_bound_name)
}

# Load, process and save South American countries data
samer_countries_filename <- "Data/samer_countries.gpkg"
# Check if South American countries dataset exists; if not, get data from GADM, process and save

if (!file.exists(samer_countries_filename))
{
  
  bra <- getData("GADM", country = "BRA", level = 0)
  per <- getData("GADM", country = "PER", level = 0)
  arg <- getData("GADM", country = "ARG", level = 0)
  bol <- getData("GADM", country = "BOL", level = 0)
  col <- getData("GADM", country = "COL", level = 0)
  ven <- getData("GADM", country = "VEN", level = 0)
  sur <- getData("GADM", country = "SUR", level = 0)
  frg <- getData("GADM", country = "GUF", level = 0)
  guy <- getData("GADM", country = "GUY", level = 0)
  ecu <- getData("GADM", country = "ECU", level = 0)
  chi <- getData("GADM", country = "CHL", level = 0)
  uru <- getData("GADM", country = "URY", level = 0)
  par <- getData("GADM", country = "PRY", level = 0)
  
  samer_countries <- rbind(
    arg, bra, uru, per, bol, col, ecu, par, chi, ven, sur, frg, guy
  ) %>% 
    st_as_sf() %>% 
    transmute(
      ID_0, ISO, NAME_ENGLISH 
    ) 
  
  st_write(samer_countries, samer_countries_filename)
  
}

# Load, process and save various geographic datasets (natural earth, amazon biome, etc.)
south_america_shape <- st_read("Data/samer_continent.gpkg")
natural_earth_filename_small <- "Data/NE_samer_small.tif"


if (!file.exists(natural_earth_filename))
{
  
  natural_earth <- raster("Data_raw/NE1_50m_SR/NE1_50M_SR/NE1_50M_SR.tif")
  
  samer_large <- crop(natural_earth, extent(c(-83, -33, -35, 12)))
  
  samer_small <- crop(natural_earth, extent(c(-82, -34, -21, 12)))
  #
  samer_forests <- samer_small
  
  samer_forests[samer_forests >= 181] <- NA
  samer_forests[samer_forests <= 149] <- NA
  
  # plot(samer_forests)
  
  #
  samer_large[samer_large >= 251] <- NA
  samer_small[samer_small < 251] <- NA
  # plot(samer_small)
  
  samer_small_cropped <- mask(samer_small, samer_countries)
  # plot(samer_small_cropped)
  samer_seas_cropped <- mask(samer_small, south_america_shape, inverse = TRUE)
  # plot(samer_seas_cropped)
  
  writeRaster(samer_small, "Data/Sea_mask.tif")
  
  amazon_biome <- st_read("Data_raw/amapoly_ivb/amapoly_ivb.shp")
  
  samer_amazon_forest <- mask(samer_forests, amazon_biome)
  
  plot(samer_amazon_forest)
  
  writeRaster(samer_large, "Data/NE_samer_large.tif", overwrite = TRUE)
  writeRaster(samer_small_cropped, natural_earth_filename_small, overwrite = TRUE)
  writeRaster(samer_amazon_forest, "Data/NE_samer_amazon.tif", overwrite = TRUE)
  
}


# Load and process indigenous areas data
indigenous_areas_filename <- "Data/Indigenous_territories.gpkg"
# Check if indigenous areas dataset exists; if not, read, process and save
if (!file.exists(indigenous_areas_filename)) {
  indig_raw <- st_read("Data_raw/TIs/Tis_TerritoriosIndigenas.shp")
  indig_clean <- indig_raw %>% 
    transmute(
      country = pais,
      category = categoria,
      name = nombre,
      ethnic = etnias
    ) %>% 
    st_transform(st_crs(4326)) %>% 
    st_simplify()
  
  st_write(indig_clean, indigenous_areas_filename)
}
