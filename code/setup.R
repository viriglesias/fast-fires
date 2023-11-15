
###########
# Libraries

library(tidyverse)
library(sf)
library(lubridate)
library(scales)
library(ggpubr)
library(terra)
library(tidyterra)
library(scales)
library(ggspatial)
library(flextable)
library(here)

maindir = '/Users/max/Library/CloudStorage/OneDrive-Personal/mcook'
icsdir <- '/Users/max/Library/CloudStorage/OneDrive-Personal/mcook/ics209-plus-fired'
firedir <- '/Users/max/Library/CloudStorage/OneDrive-Personal/mcook/FIRED'

# Define the MODIS projection
modis.prj = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m"
# Define Lambert projection
lambert.prj <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
# Default WGS projection
wgs.prj <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

########
# Data #
########

# ICS-209-PLUS (1999-2020)
ics <- st_read(paste0(icsdir,"/data/spatial/raw/wf-incidents/ics-209-plus-2.0/ics209plus-wf_incidents_spatial_us_1999to2020.gpkg")) %>%
 filter(START_YEAR > 1999) %>%
 st_transform(st_crs(lambert.prj))

# FIRED (2001-2020). 
# "../FIRED/data/spatial/mod/event-updates/conus-ak_to2022_events_qc.gpkg"
# paste0(firedir,"/data/spatial/mod/fired_events_conus_to2021091.gpkg")
fired <- st_read(paste0(firedir,"/data/spatial/mod/fired_events_conus_to2021091.gpkg")) %>%
 filter(ig_year < 2021) %>%
 st_transform(st_crs(lambert.prj))

# ICS-FIRED (2001-2020) manually QC'd for home loss work
ics.fired <-  st_read(paste0(maindir,"/home-loss/data/spatial/mod/ics-fired_spatial_west_mod.gpkg")) %>%
 st_transform(st_crs(lambert.prj))
# Add in the rest of the country from original database of ICS-FIRED
og <- st_read(paste0(icsdir,"/data/spatial/mod/ics-fired/final/ics209plus_fired_events_combined.gpkg")) %>%
 filter(!INCIDENT_ID %in% ics.fired$INCIDENT_ID) %>%
 mutate(ig_date = as.Date(ig_date),
        last_date = as.Date(last_date),
        mx_grw_dte = as.Date(mx_grw_dte))
ics.fired <- ics.fired %>%
 bind_rows(og)
rm(og)

# Bring in MTBS perims
mtbs <- st_read(paste0(maindir,"/data/mtbs/mtbs_perimeter_data/mtbs_perims_conus.gpkg")) %>% 
 mutate(Incid_Year = lubridate::year(Ig_Date)) %>% 
 filter(Incid_Year>=2000 & Incid_Year<=2020) %>%
 st_transform(st_crs(lambert.prj))

# Boundaries
states <- st_read(paste0(maindir,"/data/boundaries/political/TIGER/tl19_us_states_conus.gpkg")) %>%
 st_transform(st_crs(lambert.prj))
