##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Resample_survey_data: Multiple species, multiple years
## Authors:       Derek Bolser, Office of Science and Technology (derek.bolser@noaa.gov)
##                Em Markowitz, Alaska Fisheries Science Center (emily.markowitz@noaa.gov)
##                Elizabeth Perl, ECS Federal contracted to Office of Science and Technology (elizabeth.gugliotti@noaa.gov)
## Description:   Resample_survey_data: Multiple species, multiple years.
## Date:          March 2025
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


PKG <- c(
  
  "devtools", 
  "remotes",
  
  # other tidyverse
  "plyr",
  "dplyr",
  "magrittr",
  "tidyr",
  "ggplot2", 
  "tibble",
  "janitor", 
  "here",
  
  # Survey data pull Specific packages
  "akgfmaps", # devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
  "coldpool", # devtools::install_github("afsc-gap-products/coldpool")
  "gapctd", # install_github("afsc-gap-products/gapctd")
  "gapindex", # devtools::install_github("afsc-gap-products/gapindex")
  "nwfscSurvey", 
  
  "jsonlite", 
  "httr", 
  "sp", 
  "RODBC", 
  
  "roxygen2", 
  "usethis",
  
  "pkgdown", 
  
  # Spatial mapping
  "sf",
  "ggspatial", 
  "FishStatsUtils", 
  "fontawesome",
  
  # API pulls
  "jsonlite", 
  "httr"
)

source("./inst/r/pkg_install.R")
base::lapply(unique(PKG), pkg_install)

# Data for package -------------------------------------------------------------

data_documentation <- function(dat, title, obj_name, author, source, details, description){
  

  column <- data.frame(
    metadata_colname = c("srvy", "trawlid", "common_name", "species_code", 
                         "total_catch_numbers", "total_catch_wt_kg", "cpue_kgkm2", 
                         "latitude_dd", "longitude_dd", 
                         "year", "pass", "bottom_temperature_c", "depth_m", 
                         "geometry", "stratum", "area_km2"), 
    metadata_colname_long = c("Abbreviated survey names", "Trawl ID", "Taxon common name", "Taxon scientific name", 
                              "Taxon count", "Specimen weight (g)", "Weight CPUE (kg/km2)", 
                              "Latitude (decimal degrees)", "Longitude (decimal degrees)", 
                              "Survey year", "Pass", "Bottom temperature (degrees Celsius)", "Depth (m)", 
                              "Spatial geometry", "Stratum", "Area (km2)"), 
    metadata_colname_desc = c("Abbreviated survey names. ", 
                              "This is a unique numeric identifier assigned to each (vessel, cruise, and haul) combination.", 
                              "The common name of the marine organism associated with the scientific_name and species_code columns.", 
                              "The species code of the organism associated with the common_name and scientific_name columns.", 
                              "Total whole number of individuals caught in haul or samples collected.", 
                              "Weight of specimen (grams).", 
                              "Catch weight (kilograms) per unit effort (area swept by the net, units square kilometers).", 
                              "Latitude (one hundred thousandth of a decimal degree).", 
                              "Longitude (one hundred thousandth of a decimal degree).", 
                              "Year the observation (survey) was collected.", 
                              "Pass", 
                              "Bottom temperature (tenths of a degree Celsius); NA indicates removed or missing values.", 
                              "Bottom depth (meters).", 
                              "Spatial geometry.", 
                              "Statistical area for analyzing data. Strata are often designed using bathymetry and other geographic and habitat-related elements. The strata are unique to each survey region.", 
                              "Area in square kilometers."
    )
  )
  
  column <- column[names(column) %in% names(dat),]
  
  str0 <- paste0("#' @title ", title,"
#' @description ",description, "
#' @usage data('",obj_name,"')
#' @author ",author,"
#' @format A data frame with ",nrow(dat)," observations on the following ",
ncol(dat)," variables.
#' \\describe{
",
paste0(paste0("#'   \\item{\\code{",column$metadata_colname,"}}{", column$metadata_colname_long, ". ", column$metadata_colname_desc,"}"), collapse = "\n"),
"#'   }
#' @source ",source,"
#' @keywords species code data
#' @examples
#' data(",obj_name,")
#' @details ",details,"

'",obj_name,"'")
  
  write.table(str0,
              file = here::here("R",paste0(obj_name, ".R")),
              sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
}

source(here::here("inst", "r", "data_dl_nw.R"))
source(here::here("inst", "r", "data_dl_ne.R"))
source(here::here("inst", "r", "data_dl_ak.R"))

# README -----------------------------------------------------------------------

# Update README.Rmd file with new date version number!!!
rmarkdown::render(here::here("inst", "r", "README.Rmd"),
                  output_dir = "./",
                  output_file = "README.md")

# Update DESCRIPTION -----------------------------------------------------------
date0 <- "0.0.1"
aaa <- readLines(con = "DESCRIPTION")
aaa[grepl(pattern = "Version: ", x = aaa)] <- paste0("Version: ", date0)
write.table(x = aaa, file = "DESCRIPTION", quote = FALSE, row.names = FALSE, col.names = FALSE)

# Document and create Package --------------------------------------------------

.rs.restartR()

PKG <- c("devtools", # # devtools::install_github("rstudio/fontawesome", force = T)
         "here", 
         "usethis", 
         "roxygen2", 
         "RODBC")
source("./inst/r/pkg_install.R")
base::lapply(unique(PKG), pkg_install)

devtools::document()
setwd("..")
install("surveyresamplr")
3
setwd(here::here())
devtools::check()

## Create Documentation GitHub-Pages -------------------------------------------

.rs.restartR()

PKG <- c("fontawesome", # # devtools::install_github("rstudio/fontawesome", force = T)
         "here", 
         "usethis", 
         "pkgdown")
source("./inst/r/pkg_install.R")
base::lapply(unique(PKG), pkg_install)

# devtools::install_github("r-lib/pkgdown")
# pkgdown::build_favicons()
# devtools::build_vignettes()
# usethis::use_pkgdown(config_file = "./pkgdown/_pkgdown.yml")
# usethis::use_vignette("my-vignette")
# pkgdown::clean_site()
pkgdown::build_site(pkg = here::here())
# usethis::use_github_action("pkgdown")

# Save Package tar.gz
# date0 <- "0.0.1"
# devtools::build(path = here::here(paste0("surveyresamplr_",date0,".tar.gz")))


