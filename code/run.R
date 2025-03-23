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

# Define study species ---------------------------------------------------------

# Alaska
test_species <- data.frame(
  srvy = "EBS",
  common_name = c("walleye pollock", "snow crab", "Pacific cod", 
                  "red king crab", "blue king crab", 
                  "yellowfin sole", "Pacific halibut", 
                  "Alaska plaice", "flathead sole", "northern rock sole"), 
  species_code = as.character(c(21740, 68580, 21720, 
                                69322, 69323, 
                                10210, 10120, 
                                10285, 10130, 10261), 
                              seq_from = 0.2, 
                              seq_to = 1.0, 
                              seq_by = 0.2, 
                              tot_dataframes = 13), 
  test = "partial") %>% 
  dplyr::mutate( 
    file_name = gsub(pattern = " ", replacement = "_", x = (tolower(common_name)))
  )


# California
test_species <- dplyr::bind_rows(
  data.frame(srvy = "CA", 
             common_name = c("arrowtooth flounder", "bocaccio", "canary rockfish", "darkblotched rockfish", 
                             "Dover sole", "lingcod", "lingcod", "longnose skate", 
                             "Pacific ocean perch", "Pacific spiny dogfish", 
                             "petrale sole", "rex sole", "sablefish", 
                             "shortspine thornyhead", "yellowtail rockfish", "widow rockfish"), 
             filter_lat_gt = c(34, NA, NA, 335, NA, 35, NA, NA, 35, NA, NA, NA, NA, NA, 35.5, 33.5), 
             filter_lat_lt = c(NA, NA, NA, NA, NA, NA, 35, NA, NA, NA, NA, NA, NA, NA, NA, NA), 
             filter_depth = c(NA, 500, 275, 675, NA, 450, 450, NA, 500, 700, 675, 700, NA, NA, 425, 675), 
             test = "full", 
             seq_from = 0.1, 
             seq_to = 1.0, 
             seq_by = 0.1, 
             tot_dataframes = 91) %>% 
    dplyr::mutate( 
      file_name = gsub(pattern = " ", replacement = "_", x = (tolower(common_name))), 
      species_code = common_name), 
  test_species) 

# Set directories --------------------------------------------------------------
#setwd("C:/Users/Derek.Bolser/Documents/Resample_survey_data/") #for local testing
wd <- "Z:/Projects/Resample-survey-data/"
wd_results <- paste0(wd, "/output/")
dir.create(wd_results, showWarnings = FALSE)
wd_results_v <- paste0(wd_results, "/", Sys.Date() ,"/")
dir.create(wd_results_v, showWarnings = FALSE)

# Load survey data -------------------------------------------------------------

## Alaska ----------------------------------------------------------------------

# source(paste0(wd, "code/data_dl_ak.r"))
load(file = paste0(wd, "/data/noaa_afsc_cpue.rda"))
catch_ak <- noaa_afsc_cpue

## California ------------------------------------------------------------------

catch_ca <- read.csv(paste0(wd,"data/nwfsc_bt_fmp_spp_updated.csv")) #pulled data again to get 2024
catch_ca <- catch_ca %>% 
  dplyr::select(Trawl_id, Common_name, Longitude_dd, Latitude_dd, Year, Pass, total_catch_wt_kg) %>% 
  dplyr::mutate(srvy = "CA")

## Combine ---------------------------------------------------------------------

catch <- dplyr::bind_rows(catch_ak, catch_ca)

# Load grid data -------------------------------------------------------------

## Alaska ----------------------------------------------------------------------

load(paste0(wd,"data/noaa_afsc_ebs_pred_grid_depth_temp.rdata"))
noaa_afsc_ebs_pred_grid_depth_temp <- noaa_afsc_ebs_pred_grid_depth_temp %>% 
  dplyr::mutate(srvy = "EBS")
#make gridyrs
grid_yrs_ebs <- replicate_df(noaa_afsc_ebs_pred_grid_depth_temp, "Year", unique(catch$Year))

## California ------------------------------------------------------------------

load(paste0(wd,"data/california_current_grid.rda"))
## set up grid
#rename x and y cols
california_current_grid <- california_current_grid %>% 
  dplyr::select(Longitude_dd = longitude, 
                Latitude_dd = latitude, 
                Pass = pass_scaled, 
                Depth_m = depth) %>% 
  dplyr::mutate(srvy = "CA")
#make gridyrs
grid_yrs_ca <- replicate_df(california_current_grid, "Year", unique(catch$Year))

## Combine ---------------------------------------------------------------------

grid_yrs <- dplyr::bind_rows(grid_yrs_ca, grid_yrs_ebs)

#saveRDS(grid_yrs,"grid_yrs.rds")
write_parquet(x = grid_yrs, paste0(wd, "data/grid_yrs.parquet"))
rm(grid_yrs,california_current_grid, grid_yrs_ca, noaa_afsc_ebs_pred_grid_depth_temp, grid_yrs_ebs)

#get rid of memory limits
options(future.globals.maxSize = 1 * 1024^4)  # Allow up to 1 TB for globals

# Load support files -----------------------------------------------------------

source(paste0(wd, "code/smaller_functions_updated.R")) #need to edit to specify the code directory if running locally
# source(paste0(wd, "code/smaller_functions.R")) #need to edit to specify the code directory if running locally
source(paste0(wd, "code/cleanup_by_species.R"))
source(paste0(wd, "code/species_sdms.R"))

# Run scenarios ----------------------------------------------------------------
for (ii in 1:nrow(test_species)) {
  file <- test_species$file_name[ii]
  dir_spp <- paste0(wd_results_v, paste0(test_species$srvy[ii], "_", file, "/"))
  dir.create(dir_spp, showWarnings = FALSE)

  # # Define output file paths
  # fit_df_path <- paste0(dir_spp, paste0(file, "_fit_df.csv"))
  # fit_pars_path <- paste0(dir_spp, paste0(file, "_pars_df.csv"))
  # fit_check_path <- paste0(dir_spp, paste0(file, "_fit_check_df.csv"))
  
  all_fit_df <- list()
  all_fit_pars <- list()
  all_fit_check <- list()
  
  # Systematically filter data 
  df <- catch %>% 
    dplyr::filter(
      srvy == test_species$srvy[ii], 
      Common_name == test_species$common_name[ii])
  
  if (!is.na(test_species$filter_lat_lt[ii])) {
    df <- df %>% dplyr::filter(Latitude_dd < test_species$filter_lat_lt[ii])
  }
  if (!is.na(test_species$filter_lat_gt[ii])) {
    df <- df %>% dplyr::filter(Latitude_dd > test_species$filter_lat_gt[ii])
  }
  if (!is.na(test_species$filter_depth[ii])) {
    df <- df %>% dplyr::filter(Depth_m < test_species$filter_depth[ii])
  }
  
  spp_dfs <- cleanup_by_species(
    df = df, 
    species = test_species$common_name[ii], 
    seq_from = test_species$seq_from[ii], 
    seq_to = test_species$seq_to[ii], 
    seq_by = test_species$seq_by[ii], 
    tot_dataframes = test_species$tot_dataframes[ii])
  # if (!is.na(test_species$filter_lat_lt[ii])) {
  #   spp_dfs <- lapply(spp_dfs, 
  #                     function(x, val){x[x$Latitude_dd > val, ]}, 
  #                     val = test_species$filter_lat_lt[ii])
  # }
  # if (!is.na(test_species$filter_lat_gt[ii])) {
  #   spp_dfs <- lapply(spp_dfs, 
  #                     function(x, val){x[x$Latitude_dd < val, ]}, 
  #                     val = test_species$filter_lat_gt[ii])
  # }
  # if (!is.na(test_species$filter_depth[ii])) {
  #   spp_dfs <- lapply(spp_dfs, 
  #                     function(x, val){x[x$Depth_m > val, ]}, 
  #                     val = test_species$filter_depth[ii])
  # }
  spp_dfs <- spp_dfs[90:91] # reduce DFs for testing
  
  # make the names file
  spp_files <- as.list(names(spp_dfs))
  
  # Save each dataframe separately
  for (i in seq_along(spp_dfs)) {
    write_parquet(spp_dfs[[i]], paste0(dir_spp, paste0("df_", i, ".parquet")))
  }
  
  # Optional: Remove from memory
  rm(spp_dfs)
  gc()
  
  #set up parallel processing
  plan(callr, workers = 6)  # Adjust the number of workers based on available memory
  
  # Remove large objects before parallel execution
  gc()
  
  print("Starting parallel SDM processing")
  
  # Run SDMs in parallel
  future_map(seq_along(spp_files), function(i) {
    gc()  # Free memory
    # Load only the required dataframe
    spp_df <- read_parquet(paste0(dir_spp, paste0("df_", i, ".parquet")))
    # Load grid_yrs once
    grid_yrs <- read_parquet(paste0(wd, "data/grid_yrs.parquet"))
    # Run species SDM function
    fit <- species_sdm_fn(x = spp_df, y = spp_files[[i]], z = grid_yrs)
    # Ensure extracted objects are dataframes, Store results in lists
    all_fit_df[[file]] <- as.data.frame(fit_df_fn(fit))
    all_fit_pars[[file]] <- as.data.frame(fit_pars_fn(fit))
    all_fit_check[[file]] <- as.data.frame(fit_check_fn(fit))
    # Free memory
    rm(fit)
    # Explicitly remove objects after processing
    rm(spp_df, grid_yrs)
    gc()
    NULL
  }, .progress = TRUE, .options = furrr_options(seed = TRUE))
  
  print("Parallel SDM processing complete")
  
  ##### process fit files
  # process_and_save_fits(dir_spp, test_species$file_name[ii])
  # Combine lists into single dataframes
  final_fit_df <- if (length(all_fit_df) > 0) rbindlist(all_fit_df, fill = TRUE) else NULL
  final_fit_pars <- if (length(all_fit_pars) > 0) rbindlist(all_fit_pars, fill = TRUE) else NULL
  final_fit_check <- if (length(all_fit_check) > 0) rbindlist(all_fit_check, fill = TRUE) else NULL
  
  # Write to CSV if there is data
  if (!is.null(final_fit_df)) fwrite(final_fit_df, file = fit_df_path)
  if (!is.null(final_fit_pars)) fwrite(final_fit_pars, file = fit_pars_path)
  if (!is.null(final_fit_check)) fwrite(final_fit_check, file = fit_check_path)
  
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
