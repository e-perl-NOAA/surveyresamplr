# NOTES ------------------------------------------------------------------------
# Resample_survey_data: Multiple species, multiple years
# Em Markowitz March 2025
# Alaska Fisheries
#### Resample_survey_data: Multiple species, multiple years
# ------------------------------------------------------------------------------

# Install Libraries ------------------------------------------------------------

# Here we list all the packages we will need for this whole process
# We'll also use this in our works cited page!!!
PKG <- c(
  
  "devtools", 
  # "require", 
  
  # other tidyverse
  "here",
  "plyr",
  "dplyr",
  "magrittr",
  "tidyr",
  "readxl", 
  "here",
  "viridis",
  # "janitor",
  "readr",
  "ggplot2", 
  "tibble",
  "janitor", 
  "data.table", 
  
  # RACE-GAP Specific
  "akgfmaps", # devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
  "coldpool", # devtools::install_github("afsc-gap-products/coldpool")
  
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

PKG <- unique(PKG)
for (p in PKG) {
  if(!require(p, character.only = TRUE)) {
    if (p == 'coldpool') {
      devtools::install_github("afsc-gap-products/coldpool")
    } else if (p == "akgfmapas") {
      devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
    } else {
      install.packages(p)
    }
    require(p,character.only = TRUE)}
}


#### set wds
#setwd("C:/Users/Derek.Bolser/Documents/Resample_survey_data/") #for local testing
wd<- "Z:/Projects"
# wd <- "/home/user"
basedir<-file.path(wd,'Resample-survey-data')
output <- file.path(wd, "Resample-survey-data/Results")
data <-file.path(wd, "Resample-survey-data/data")
arrowtooth <- file.path(output, "Arrowtooth_flounder")
bocaccio <- file.path(output, "Bocaccio")
canary <- file.path(output, "Canary_rockfish")
darkblotched <- file.path(output, "Darkblotched_rockfish")
dover <- file.path(output, "Dover_sole")
lingcod_n <- file.path(output, "Lingcod_north")
lingcod_s <- file.path(output, "Lingcod_south")
longnose <- file.path(output, "Longnose_skate")
pop <- file.path(output, "Pacific_ocean_perch")
dogfish <- file.path(output, "Pacific_spiny_dogfish")
petrale <- file.path(output, "Petrale_sole")
rex <- file.path(output, "Rex_sole")
sablefish <- file.path(output, "Sablefish")
shortspine <- file.path(output, "Shortspine_thornyhead")
widow <- file.path(output, "Widow_rockfish")
yellowtail <- file.path(output, "Yellowtail_rockfish")

# Define study species ---------------------------------------------------------

test_species <- data.frame(
  SRVY = "BS",
  common_name = c("walleye pollock", "snow crab", "Pacific cod", 
                  "red king crab", "blue king crab", 
                  "yellowfin sole", "Pacific halibut", 
                  "Alaska plaice", "flathead sole", "northern rock sole"), 
  species_code = c(21740, 68580, 21720, 
                   69322, 69323, 
                   10210, 10120, 
                   10285, 10130, 10261)) %>% 
  dplyr::mutate( 
    file_name = gsub(pattern = " ", replacement = "_", x = (tolower(common_name)))
  )

test_species <- dplyr::bind_rows(
  data.frame(SRVY = "CA", 
             common_name = "arrowtooth flounder", 
             species_code = NA, 
             file_name = "arrowtooth_flounder"), 
  test_species)

# Set directories --------------------------------------------------------------
wd <- "Z:/Projects/Resample-survey-data/"
wd_results <- paste0(wd, "/Results/alaska/")
dir.create(wd_results, showWarnings = FALSE)
wd_results_v <- paste0(wd_results, "/", Sys.Date() ,"/")
dir.create(wd_results_v, showWarnings = FALSE)

# read in data
#catch <- read.csv(file.path(data,"nwfsc_bt_fmp_spp.csv"))
catch <- read.csv(file.path(data,"nwfsc_bt_fmp_spp_updated.csv")) #pulled data again to get 2024
load(file.path(data,"california_current_grid.rda"))

source(file.path(basedir, "smaller_functions.R")) #need to edit to specify the code directory if running locally
source(file.path(basedir, "cleanup_by_species.R"))
source(file.path(basedir, "species_sdms.R"))

# set up grid
#rename x and y cols
california_current_grid$Longitude_dd<- california_current_grid$longitude
california_current_grid$Latitude_dd<- california_current_grid$latitude
california_current_grid$Pass<- california_current_grid$pass_scaled
california_current_grid$Depth_m<- california_current_grid$depth

#make gridyrs
grid_yrs <- replicate_df(california_current_grid, "Year", unique(catch$Year))
setwd(basedir)
#saveRDS(grid_yrs,"grid_yrs.rds")
write_parquet(grid_yrs, "grid_yrs.parquet")
rm(grid_yrs,california_current_grid)

#get rid of memory limits
options(future.globals.maxSize = 1 * 1024^4)  # Allow up to 1 TB for globals

# Run scenarios ----------------------------------------------------------------
for (ii in 1:nrow(test_species)) {
  dir_spp <- paste0(wd_results_v, test_species$file_name[ii])
  dir.create(dir_spp, showWarnings = FALSE)
  setwd(dir_spp)

  spp_dfs <- cleanup_by_species(df = catch, species = test_species$common_name[ii])
  spp_dfs <- lapply(spp_dfs, lat_filter_34)
  #spp_dfs <- spp_dfs[90:91] # reduce DFs for testing
  
  # make the names file
  spp_files <- as.list(names(spp_dfs))
  
  #save some space
  setwd(dir_spp)

  # Save each dataframe separately
  for (i in seq_along(spp_dfs)) {
    write_parquet(spp_dfs[[i]], file.path(dir_spp, paste0("df_", i, ".parquet")))
  }
  
  # Optional: Remove from memory
  rm(spp_dfs)
  gc()
  
  #set up parallel processing
  plan(callr, workers = 6)  # Adjust the number of workers based on available memory
  
  # Remove large objects before parallel execution
  gc()
  
  print("Starting parallel SDM processing")
  setwd(dir_spp)
  
  # Run SDMs in parallel
  future_map(seq_along(spp_files), function(i) {
    gc()  # Free memory
    # Load only the required dataframe
    spp_df <- read_parquet(file.path(dir_spp, paste0("df_", i, ".parquet")))
    # Load grid_yrs once
    grid_yrs <- read_parquet(file.path(basedir, "grid_yrs.parquet"))
    # Run species SDM function
    species_sdm_fn(spp_df, spp_files[[i]], grid_yrs)
    # Explicitly remove objects after processing
    rm(spp_df, grid_yrs)
    gc()
    NULL
  }, .progress = TRUE,.options = furrr_options(seed = TRUE))
  
  print("Parallel SDM processing complete")
  
  ##### process fit files
  process_and_save_fits(spp,"spp")
  
  ##### process index files
  spp_indices <- pull_files(spp, "index")
  spp_indices_df <- bind_index_fn(spp_indices)
  write.csv(spp_indices_df, "spp_indices_df.csv", row.names = F)
  
  # Remove the rest of the files
  rm("spp_files", "spp_indices", "spp_indices_df")
  
  #remove from memory
  files_to_keep <- c("spp_fit_check_df.csv", "spp_fit_df.csv", "spp_pars_df.csv", "spp_indices_df.csv")
  all_files <- list.files(path = ".", full.names = TRUE)  # Get all files
  files_to_remove <- setdiff(all_files, file.path(".", files_to_keep))  # Exclude files to keep
  file.remove(files_to_remove)  # Delete the files
}