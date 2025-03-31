##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Resample_survey_data: Multiple species, multiple years
## Authors:       Derek Bolser, Office of Science and Technology (derek.bolser@noaa.gov)
##                Em Markowitz, Alaska Fisheries Science Center (emily.markowitz@noaa.gov)
##                Elizabeth Perl, ECS Federal contracted to Office of Science and Technology (elizabeth.gugliotti@noaa.gov)
## Description:   Resample_survey_data: Multiple species, multiple years.
## Date:          March 2025
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#get rid of memory limits
options(future.globals.maxSize = 1 * 1024^4)  # Allow up to 1 TB for globals

# Set directories --------------------------------------------------------------
library(here)
# wd <- "C:/Users/Derek.Bolser/Documents/Resample_survey_data/" #for local testing
# wd <- "Z:/Projects/Resample-survey-data/"
wd <- paste0(here::here(),"/")
dir_out <- paste0(wd, "/output/")
dir.create(dir_out, showWarnings = FALSE)

# Load support files -----------------------------------------------------------

source(paste0(wd, "code/functions.R"))
source(paste0(wd, "code/functions_sdms.R"))

# Install Libraries ------------------------------------------------------------

# Here we list all the packages we will need for this whole process
# We'll also use this in our works cited page. 
PKG <- c(
  
  "devtools", 
  # "require", 

  # other tidyverse
  "plyr",
  "dplyr",
  "magrittr",
  "tidyr",
  "readxl", 
  "viridis",
  "readr",
  "ggplot2", 
  "tibble",
  "janitor", 
  "data.table", 
  
  # Survey data pull Specific packages
  "akgfmaps", # devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
  "coldpool", # devtools::install_github("afsc-gap-products/coldpool")
  "nwfscSurvey", # remotes::install_github("pfmc-assessments/nwfscSurvey")
  "jsonlite", 
  "httr", 
  "sp", 
  "RODBC", 
  
  # Spatial mapping
  "sf",
  "ggspatial", 
  
  # API pulls
  "jsonlite", 
  "httr",
  
  # parallelizing
  "forcats",
  "purrr",
  "furrr", 
  "doParallel",
  
  # sampling
  "sampling",
  
  # modeling
  "arrow", 
  "future.apply", 
  "future.callr", 
  "sdmTMB", # install.packages("remotes"",; remotes::install_github("pbs-assess/sdmTMBextra", dependencies = TRUE",
  "Matrix", 
  "MASS",
  "cluster", 
  "TMB", 
  "INLA" # install.packages("INLA",repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
  # inla.upgrade() # for the stable version
  # Windows will not update a package already loaded, so then you have to remove the package and install it from scratch. There are two suggested packages, ‘graph’ and ‘Rgraphviz’, that are on Bioconductor, and you can install those with:
  #   
  #   if (!requireNamespace("BiocManager", quietly = TRUE))
  #     install.packages("BiocManager")
  # BiocManager::install(c("graph", "Rgraphviz"), dep=TRUE)
  
  
  # options(repos=c( inlabruorg = "https://inlabru-org.r-universe.dev", INLA = "https://inla.r-inla-download.org/R/testing", CRAN = "https://cran.rstudio.com") )
  # install.packages("fmesher")
  
)

# Use pkg_install() function found in functions.R file to load packages
lapply(unique(PKG), pkg_install)

# Update README (sometimes) ----------------------------------------------------

# rmarkdown::render(here::here("code/README.Rmd"),
#                   output_dir = "./",
#                   output_file = here::here("README.md"))

# Run scenarios ----------------------------------------------------------------

## NWFSC California Current ----------------------------------------------------

### Define study species -------------------------------------------------------

test_species <- data.frame(
  srvy = "CA", 
  common_name = c("arrowtooth flounder", "bocaccio", "canary rockfish", "darkblotched rockfish", 
                  "Dover sole", "lingcod", "lingcod", "longnose skate", 
                  "Pacific ocean perch", "Pacific spiny dogfish", 
                  "petrale sole", "rex sole", "sablefish", 
                  "shortspine thornyhead", "yellowtail rockfish", "widow rockfish"), 
  file_name = c("arrowtooth_flounder", "bocaccio", "canary_rockfish", "darkblotched_rockfish", 
                "dover_sole", "lingcod_N", "lingcod_S", "longnose_skate", 
                "pacific_ocean_perch", "pacific_spiny_dogfish",  "petrale_sole", "rex_sole",           
                "sabefish", "shortspine_thornyhead" ,"yellowtail_rockfish", "widow_rockfish"),
  filter_lat_gt = c(34, NA, NA, 335, NA, 35, NA, NA, 35, NA, NA, NA, NA, NA, 35.5, 33.5), 
  filter_lat_lt = c(NA, NA, NA, NA, NA, NA, 35, NA, NA, NA, NA, NA, NA, NA, NA, NA), 
  filter_depth = c(NA, 500, 275, 675, NA, 450, 450, NA, 500, 700, 675, 700, NA, NA, 425, 675), 
  model_fn = c( # name of funcion for sdm. Will build in specificity for this later
    "species_sdm_fn", "species_sdm_fn", "canary_sdm_fn", "darkblotched_sdm_fn", 
    "species_sdm_fn", "species_sdm_fn", "species_sdm_fn", "species_sdm_fn", 
    "species_sdm_fn", "species_sdm_fn", 
    "species_sdm_lognormal_fn", "species_sdm_fn", "species_sdm_lognormal_fn", 
    "shortspine_sdm_fn", "species_sdm_fn", "species_sdm_fn")
) 

### Load survey data -----------------------------------------------------------

# source(paste0(wd, "code/data_dl_nw.r"))

catch_ca <- read.csv(paste0(wd,"data/nwfsc_bt_fmp_spp_updated.csv")) #pulled data again to get 2024
catch_ca <- catch_ca %>% 
  dplyr::select(Trawl_id, Common_name, Longitude_dd, Latitude_dd, Year, Pass, total_catch_wt_kg, Depth_m) %>% 
  dplyr::mutate(srvy = "CA")

catch_ca <- catch_ca %>% dplyr::filter(Year > 2010) # Testing

### Load grid data -------------------------------------------------------------

load(paste0(wd,"data/california_current_grid.rda"))
california_current_grid <- california_current_grid %>% # rename x and y cols
  dplyr::select(Longitude_dd = longitude, 
                Latitude_dd = latitude, 
                Pass = pass_scaled, 
                Depth_m = depth, 
                area_km2 = area_km2_WCGBTS) %>% 
  dplyr::mutate(srvy = "CA")
grid_yrs_ca <- replicate_df(california_current_grid, "Year", unique(catch_ca$Year))

### Variables ------------------------------------------------------------------

seq_from = 0.1
seq_to = 1
seq_by = 0.1
tot_dataframes = 91
replicate_num <- 10
catch <- catch_ca
grid_yrs <- grid_yrs_ca
srvy <- "CA"

### Run ------------------------------------------------------------------------

sink(file = paste0(dir_out, srvy, "_", Sys.Date(), "_logfile.txt"), append=FALSE, split=TRUE)  # for screen and log
map(
  1:nrow(test_species), 
  ~ clean_and_resample(test_species[.x,], 
                       catch, seq_from, seq_to, seq_by, 
                       tot_dataframes, replicate_num, grid_yrs, dir_out))
sink()

### Plot indices --------------------------------------------------------------

plot_results(srvy = srvy, dir_out = dir_out) 

## Alaska ----------------------------------------------------------------------

### Define study species -------------------------------------------------------

test_species <- data.frame(
  srvy = "EBS",
  common_name = c("walleye pollock", "snow crab", "Pacific cod", 
                  "red king crab", "blue king crab", 
                  "yellowfin sole", "Pacific halibut", 
                  "Alaska plaice", "flathead sole", "northern rock sole"), 
  species_code = as.character(c(21740, 68580, 21720, 
                                69322, 69323, 
                                10210, 10120, 
                                10285, 10130, 10261)), 
  filter_lat_lt = NA, 
  filter_lat_gt = NA, 
  filter_depth = NA, 
  model_fn = "species_sdm_fn" # name of funcion for sdm. Will build in specificity for this later
) %>% 
  dplyr::mutate( 
    file_name = gsub(pattern = " ", replacement = "_", x = (tolower(common_name)))  )

### Load survey data -----------------------------------------------------------

# source(paste0(wd, "code/data_dl_ak.r"))

load(file = paste0(wd, "/data/noaa_afsc_cpue.rda"))
catch_ak <- noaa_afsc_cpue
catch_ak <- catch_ak %>% dplyr::filter(Year > 2010) # Testing

### Load grid data -------------------------------------------------------------

load(paste0(wd, "data/noaa_afsc_ebs_pred_grid_depth.rdata"))
noaa_afsc_ebs_pred_grid_depth <- noaa_afsc_ebs_pred_grid_depth %>% 
  dplyr::mutate(srvy = "EBS")
grid_yrs_ebs <- replicate_df(noaa_afsc_ebs_pred_grid_depth, "Year", unique(catch_ak$Year))

### Variables ------------------------------------------------------------------

seq_from = 0.2
seq_to = 1.0
seq_by = 0.2 
tot_dataframes = 13
replicate_num <- 3
catch <- catch_ak
grid_yrs <- grid_yrs_ebs
srvy <- "EBS"

### Run ------------------------------------------------------------------------

sink(file = paste0(dir_out, srvy, "_", Sys.Date(), "_logfile.txt"), append=FALSE, split=TRUE)  # for screen and log
map(
  1:nrow(test_species), 
  ~ clean_and_resample(test_species[.x,], 
                       catch, seq_from, seq_to, seq_by, 
                       tot_dataframes, replicate_num, grid_yrs, dir_out))
sink()

### Plot indices --------------------------------------------------------------

plot_results(srvy = srvy, dir_out = dir_out) 

