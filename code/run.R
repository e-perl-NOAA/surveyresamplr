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
                                10285, 10130, 10261))) %>% 
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
             filter_depth = c(NA, 500, 275, 675, NA, 450, 450, NA, 500, 700, 675, 700, NA, NA, 425, 675)
  ) %>% 
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

load(paste0(wd, "data/noaa_afsc_ebs_pred_grid_depth.rdata"))
noaa_afsc_ebs_pred_grid_depth <- noaa_afsc_ebs_pred_grid_depth %>% 
  dplyr::mutate(srvy = "EBS")
#make gridyrs
grid_yrs_ebs <- replicate_df(noaa_afsc_ebs_pred_grid_depth, "Year", unique(catch_ak$Year))

## California ------------------------------------------------------------------

load(paste0(wd,"data/california_current_grid.rda"))
## set up grid
#rename x and y cols
california_current_grid <- california_current_grid %>% 
  dplyr::select(Longitude_dd = longitude, 
                Latitude_dd = latitude, 
                Pass = pass_scaled, 
                Depth_m = depth, 
                area_km2 = area_km2_WCGBTS) %>% 
  dplyr::mutate(srvy = "CA")
#make gridyrs
grid_yrs_ca <- replicate_df(california_current_grid, "Year", unique(catch_ca$Year))

## Combine ---------------------------------------------------------------------

grid_yrs <- dplyr::bind_rows(grid_yrs_ca, grid_yrs_ebs)

#saveRDS(grid_yrs,"grid_yrs.rds")
write_parquet(x = grid_yrs, paste0(wd, "data/grid_yrs.parquet"))
rm(california_current_grid, grid_yrs_ca, noaa_afsc_ebs_pred_grid_depth, grid_yrs_ebs)

#get rid of memory limits
options(future.globals.maxSize = 1 * 1024^4)  # Allow up to 1 TB for globals

# Load support files -----------------------------------------------------------

source(paste0(wd, "code/smaller_functions_updated.R")) #need to edit to specify the code directory if running locally
# source(paste0(wd, "code/smaller_functions.R")) #need to edit to specify the code directory if running locally
source(paste0(wd, "code/cleanup_by_species.R"))
source(paste0(wd, "code/species_sdms.R"))

# Additional functions ---------------------------------------------------------

#' Include or Exclude
#'
#' Specify how to downsample. For simple random sampling, a proportion of
#' stations should do.
#'
#' @param df tows data frame
#' @param proportions proportions developed using: props <- as.data.frame(seq
#' (0.1,1.0, by = 0.1)) replicated by the length of the tows dataframe. The name
#' of the props is "Trawl_id".
#'

include_or_exclude <- function(df, proportions, replicate_num = 10) {
  # Get the number of rows in the dataframe
  num_rows <- nrow(df)
  
  # Use lapply to create a list of dataframes
  result_list <- lapply(proportions, function(p) {
    # Generate a random vector of 1s and 0s based on the specified proportion
    set.seed(1)
    random_vectors <- replicate(replicate_num, sample(c(1, 0), size = num_rows, replace = TRUE, prob = c(p, 1 - p)), simplify = F)
    
    # Create a new dataframe with the random assignments
    lapply(random_vectors, function(rv) {
      cbind(df, RandomAssignment = rv)
    })
  })
  
  # flatten into single list
  result_list <- do.call(c, result_list)
  
  # Set names for the list elements based on proportions
  names(result_list) <- paste0(rep(proportions, each = 10), "_", rep(1:10, times = length(proportions)))
  
  # Return the list of dataframes
  return(result_list)
}

include_or_exclude10 <- function(df, proportions, replicate_num = 10) {
  # Get the number of rows in the dataframe
  num_rows <- nrow(df)
  
  # Use lapply to create a list of dataframes
  result_list <- lapply(proportions, function(p) {
    # Generate a random vector of 1s and 0s based on the specified proportion
    set.seed(1)
    random_vectors <- replicate(replicate_num, sample(c(1, 0), size = num_rows, replace = TRUE, prob = c(p, 1 - p)), simplify = F)
    
    # Create a new dataframe with the random assignments
    lapply(random_vectors, function(rv) {
      cbind(df, RandomAssignment = rv)
    })
  })
  
  # flatten into single list
  result_list <- do.call(c, result_list)
  
  # Set names for the list elements based on proportions
  names(result_list) <- paste0(rep(proportions, each = 10), "_", rep(1:10, times = length(proportions)))
  
  # Return the list of dataframes
  return(result_list)
}




include_or_exclude3 <- function(df, proportions, replicate_num = 3) {
  # Get the number of rows in the dataframe
  num_rows <- nrow(df)
  
  # Use lapply to create a list of dataframes
  result_list <- lapply(proportions, function(p) {
    # Generate a random vector of 1s and 0s based on the specified proportion
    set.seed(1)
    random_vectors <- replicate(replicate_num, sample(c(1, 0), size = num_rows, replace = TRUE, prob = c(p, 1 - p)), simplify = F)
    
    # Create a new dataframe with the random assignments
    lapply(random_vectors, function(rv) {
      cbind(df, RandomAssignment = rv)
    })
  })
  
  # flatten into single list
  result_list <- do.call(c, result_list)
  
  # Set names for the list elements based on proportions
  names(result_list) <- paste0(rep(proportions, each = 10), "_", rep(1:10, times = length(proportions)))
  
  # Return the list of dataframes
  return(result_list)
}


#' Cleanup species function
#'
#' Filter catch by species, create a vector of tows and give tows a random
#' assignment to be resampled,
#'
#' @param df full catch df
#' @param species The species which you want to do data cleanup for.
#' @param tot_dataframes the number of data frames you want to output. effort x replicates - (replicates - 1). 5x3-2
#' @return List of resampled catch data frames
#'
cleanup_by_species <- function(catch, 
                               spp_info, 
                               seq_from = 0.1, 
                               seq_to = 1.0, 
                               seq_by = 0.1, 
                               tot_dataframes = 91, 
                               replicate_num = 10) {
  
  df <- catch %>% 
    dplyr::filter(
      Common_name == spp_info$common_name[ii],
      srvy == spp_info$srvy[ii])
  
  if (!is.na(spp_info$filter_lat_lt[ii])) {
    df <- df %>% dplyr::filter(Latitude_dd < spp_info$filter_lat_lt[ii])
  }
  if (!is.na(spp_info$filter_lat_gt[ii])) {
    df <- df %>% dplyr::filter(Latitude_dd > spp_info$filter_lat_gt[ii])
  }
  if (!is.na(spp_info$filter_depth[ii])) {
    df <- df %>% dplyr::filter(Depth_m < spp_info$filter_depth[ii])
  }
  
  catch_split <- split(df, df$Year)
  
  tows <- lapply(catch_split, tow_fn)
  
  # Assign random 1s and 0s based on the specified proportions to a list of dataframes
  props <- as.data.frame(seq(from = seq_from, to = seq_to, by = seq_by))
  names(props) <- "Trawl_id"
  
  # match the structure of the catch data
  props <- rep(props, length(tows))
  
  if (replicate_num == 10){
    tows_assigned <- map2(tows, props, include_or_exclude10)
  }
  if (replicate_num == 3){
    tows_assigned <- map2(tows, props, include_or_exclude3)
  }
  # tows_assigned <- purrr::pmap(list(.x = tows, .y = props, .z = replicate_num), include_or_exclude)
  
  # remove replicates of the 1 effort level
  tows_assigned <- lapply(tows_assigned, function(x) {
    x <- x[1:tot_dataframes]
    return(x)
  })
  
  tows_assigned_resampled <- purrr::map(tows_assigned, function(x) {
    purrr::map(x, function(y) {
      y[y$RandomAssignment == 1, ]
    })
  })
  
  tows_assigned_resampled <- unlist(tows_assigned_resampled, recursive = F)
  
  alldata_resampled <- join_dfs(tows_assigned_resampled, df, "Trawl_id")
  
  names(alldata_resampled) <- substr(names(alldata_resampled), 6, 50) # it would be good to replace 50 with a logical indicating the end
  
  species_all_yrs <- alldata_resampled %>%
    bind_rows(.id = "source")
  
  species_all_yrs <- split(species_all_yrs, species_all_yrs$source)
  
  rm("df", "catch_split", "tows", "props", "tows_assigned", "alldata_resampled")
  
  return(species_all_yrs)
}


resample_tests <- function (spp_dfs = spp_dfs, spp_info, grid_yrs, dir_out) {
  # set directories for outputs
  dir_spp <- paste0(dir_out, paste0(test_species$srvy[ii], "_", test_species$file_name[ii], "/"))
  dir.create(dir_spp, showWarnings = FALSE)
  
  all_fit_df <- all_fit_pars <- all_fit_check <- list()
  
  spp_dfs <- spp_dfs[90:91] # reduce DFs for testing
  spp_files <- as.list(names(spp_dfs)) # make the names file
  for (i in seq_along(spp_dfs)) { # Save each dataframe separately
    write_parquet(spp_dfs[[i]], paste0(dir_spp, paste0("df_", i, ".parquet")))
  }
  rm(spp_dfs) # Optional: Remove from memory
  gc()
  
  #set up parallel processing
  plan(callr, workers = 6)  # Adjust the number of workers based on available memory
  
  # Remove large objects before parallel execution
  gc()
  
  print("...Starting parallel SDM processing")
  
  # Run SDMs in parallel
  future_map(seq_along(spp_files), function(i) {
    gc()  # Free memory
    # Load only the required dataframe
    spp_df <- read_parquet(paste0(dir_spp, paste0("df_", i, ".parquet")))
    # Run species SDM function
    fit <- species_sdm_fn(x = spp_df, y = spp_files[[i]], z = grid_yrs)
    # Ensure extracted objects are dataframes, Store results in lists
    all_fit_df[[spp_files[[i]]]] <- cbind(test = spp_files[[i]], as.data.frame(fit_df_fn(fit)))
    fwrite(rbindlist(all_fit_df, fill = TRUE), file = fit_df_path)
    all_fit_pars[[spp_files[[i]]]] <- cbind(test = spp_files[[i]], as.data.frame(fit_pars_fn(fit)))
    fwrite(rbindlist(all_fit_pars, fill = TRUE), file = fit_pars_path)
    all_fit_check[[spp_files[[i]]]] <- cbind(test = spp_files[[i]], as.data.frame(fit_check_fn(fit)))
    fwrite(rbindlist(all_fit_check, fill = TRUE), file = fit_check_path)
    # Explicitly remove objects after processing
    rm(fit, spp_df, grid_yrs1)
    gc()
    NULL
  }, .progress = TRUE, .options = furrr_options(seed = TRUE))
  
  print("...Parallel SDM processing complete")
  
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

# Run scenarios ----------------------------------------------------------------

## California Current ----------------------------------------------------------

seq_from = 0.1
seq_to = 1
seq_by = 0.1
tot_dataframes = 91
grid_yrs1 <- grid_yrs %>% dplyr::filter(srvy == "CA")
test_species1 <- test_species %>% dplyr::filter(srvy == "CA")
replicate_num <- 10
catch <- catch_ca

for (ii in 1:nrow(test_species)){
  print(test_species$common_name[ii])
  spp_dfs <- cleanup_by_species(
    catch = catch, 
    spp_info = test_species1[ii,], 
    seq_from = seq_from, 
    seq_to = seq_to, 
    seq_by = seq_by, 
    tot_dataframes = tot_dataframes, 
    replicate_num = replicate_num)
  
  try({
    resample_tests(
      spp_dfs = spp_dfs, 
      spp_info = test_species[ii,], 
      grid_yrs = grid_yrs1, 
      dir_out = wd_results) 
  }, silent = FALSE)      # end of try function
}

## Alaska ----------------------------------------------------------------------

seq_from = 0.2
seq_to = 1.0
seq_by = 0.2 
tot_dataframes = 13
grid_yrs1 <- grid_yrs %>% dplyr::filter(srvy == "EBS")
test_species1 <- test_species %>% dplyr::filter(srvy == "EBS")
replicate_num <- 3
catch <- catch_ak %>% dplyr::filter(srvy == "EBS")

for (ii in 1:nrow(test_species)){
  print(test_species$common_name[ii])
  
  spp_dfs <- cleanup_by_species(
    catch = catch, 
    spp_info = test_species[ii,], 
    seq_from = seq_from, 
    seq_to = seq_to, 
    seq_by = seq_by, 
    tot_dataframes = tot_dataframes, 
    replicate_num = replicate_num)
  
  try({
    resample_tests(
      spp_dfs = spp_dfs, 
      spp_info = test_species[,ii], 
      grid_yrs = grid_yrs1, 
      dir_out = wd_results) 
  }, silent = FALSE)      # end of try function
}

# Scrap ------------------------------------------------------------------------

result <- 0
for(i in 1:10){
  try({                # start code block
    result = result + i
    log("a")             # I do not care about this error
    result = result + i
  }, silent=TRUE)      # end of try function
}