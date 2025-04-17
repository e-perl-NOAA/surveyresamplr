
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Resample_survey_data: Multiple species, multiple years
## Authors:       Derek Bolser, Office of Science and Technology (derek.bolser@noaa.gov)
## Description:   Resample_survey_data: Multiple species, multiple years.
## Date:          March 2025
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load support files -----------------------------------------------------------

# Here we list all the packages we will need for this whole process
# We'll also use this in our works cited page. 
PKG <- c(
  "here",
  "devtools", 
  "Require",
  "nwfscSurvey", 
  
  # other tidyverse
  "plyr",
  "dplyr",
  "magrittr",
  "tidyr",
  "readr"
)

# Use pkg_install() function found in functions.R file to load packages
source("./inst/r/pkg_install.R")
base::lapply(unique(PKG), pkg_install)

# Get cpue data ----------------------------------------------------------------

#pull BT data for all spp 
# PullCatch.fn and PullBio.fn have been deprecated, switching to pull_catch() and 
# pull_bio()
noaa_nwfsc_catch <- nwfscSurvey::pull_catch(survey = "NWFSC.Combo", 
                               common_name = c("Pacific spiny dogfish",
                                               "longnose skate",
                                               "arrowtooth flounder",
                                               "petrale sole",
                                               "rex sole",
                                               "Dover sole",
                                               "sablefish",
                                               "shortspine thornyhead",
                                               "Pacific ocean perch",
                                               "darkblotched rockfish",
                                               "widow rockfish",
                                               "yellowtail rockfish",
                                               # "chilipepper", I didn't see this one in Derek's OG species list
                                               "bocaccio",
                                               "canary rockfish",
                                               "lingcod")) %>% 
  unique() %>% 
  dplyr::select(trawlid = Trawl_id, 
                common_name = Common_name, 
                longitude_dd = Longitude_dd, 
                latitude_dd = Latitude_dd, 
                year = Year, 
                pass = Pass, 
                total_catch_wt_kg, 
                depth_m = Depth_m) %>% 
  dplyr::mutate(srvy = "CA")

# table(catch$common_name)

# Save data -------------------------------------------------------------------

#write csv
# setwd(data)
# utils::write.csv(catch,here::here("data", "nwfsc_bt_fmp_spp_updated.csv"), row.names = F)

dat <- noaa_nwfsc_catch
title <- "Combined NWFSC catch, haul, and species data"
obj_name <- "noaa_nwfsc_catch"
author <- "Northwest Fisheries Science Center, compiled by Emily Markowitz (Emily.Markowitz AT noaa.gov) and Derek Bolser (Derek.Bolser AT noaa.gov)"
source <- "Northwest Fisheries Science Center. "
details <- "[ENTER]."
description <- "[ENTER]."

save(noaa_nwfsc_catch, file = here::here("data", paste0(obj_name, ".rda")))
data_documentation(dat, title, obj_name, author, source, details, description)

# Get grid ---------------------------------------------------------------------

path <- here::here("inst", "exdata", "grids", "orig", "california_current_grid.rda")
url <- "https://raw.githubusercontent.com/pfmc-assessments/indexwc/main/data/california_current_grid.rda" # read in from indexwc

downloader::download(
  url = url,
  destfile = path
)

load(path, verbose = TRUE)

pred_grid_depth <- california_current_grid %>% # rename x and y cols
  dplyr::select(longitude_dd = longitude, 
                latitude_dd = latitude, 
                pass = pass_scaled, 
                depth_m = depth, 
                area_km2 = area_km2_WCGBTS) %>% 
  dplyr::mutate(srvy = "CA")

obj_name <- paste0("noaa_nwfsc_pred_grid_depth.rdata")
title <- paste0("Prediction grid for NWFSC survey")
obj_name <- "noaa_nwfsc_catch"
author <- "Northwest Fisheries Science Center, compiled by Emily Markowitz (Emily.Markowitz AT noaa.gov) and Derek Bolser (Derek.Bolser AT noaa.gov)"
source <- paste0("Northwest Fisheries Science Center; ",url,". ")
details <- "[ENTER]."
description <- "[ENTER]."

save(noaa_nwfsc_catch, file = here::here("data", paste0(obj_name, ".rda")))
data_documentation(dat, title, obj_name, author, source, details, description)

