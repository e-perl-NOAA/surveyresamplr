
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Resample_survey_data: Multiple species, multiple years
## Authors:       Derek Bolser, Office of Science and Technology (derek.bolser@noaa.gov)
## Description:   Resample_survey_data: Multiple species, multiple years.
## Date:          March 2025
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load support files -----------------------------------------------------------

source(paste0(wd, "code/functions.R"))

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

PKG <- unique(PKG)

lapply(PKG, pkg_install)


# Get cpue data ----------------------------------------------------------------

#define wd
wd <- getwd()
data <-file.path(wd, "Resample-survey-data/data")

#pull BT data for all spp 
# PullCatch.fn and PullBio.fn have been deprecated, switching to pull_catch() and 
# pull_bio()
catch<-nwfscSurvey::pull_catch(SurveyName = "NWFSC.Combo", 
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
                                               "lingcod")) 
#bio<-nwfscSurvey::pull_bio(SurveyName = "NWFSC.Combo")

table(catch$Common_name)
catch<-unique(catch)

#write csv
setwd(data)
write.csv(catch,here::here("data", "nwfsc_bt_fmp_spp_updated.csv"), row.names = F)


# Get grid ---------------------------------------------------------------------

# clear environment
rm(list = ls())

#read in from indexwc
#get grid
#setwd("C:/Users/Derek.Bolser/Documents/Resample_survey_data/")
path <- fs::path(getwd(),"Resample-survey-data/data", "california_current_grid.rda")
url <- "https://raw.githubusercontent.com/pfmc-assessments/indexwc/main/data/california_current_grid.rda"

downloader::download(
  url = url,
  destfile = path
)

load(path)

