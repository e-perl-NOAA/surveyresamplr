# NOTES ------------------------------------------------------------------------
# Resample_survey_data: Multiple species, multiple years
# Em Markowitz, Alaska Fisheries Science Center
# Derek Bolser, Office of Science and Technology
# March 2025
# Resample_survey_data: Multiple species, multiple years

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

# Set directories --------------------------------------------------------------
#setwd("C:/Users/Derek.Bolser/Documents/Resample_survey_data/") #for local testing
wd <- "Z:/Projects/Resample-survey-data/"
dir_out <- paste0(wd, "/output/")
dir.create(dir_out, showWarnings = FALSE)

#get rid of memory limits
options(future.globals.maxSize = 1 * 1024^4)  # Allow up to 1 TB for globals

# Load support files -----------------------------------------------------------

# source(paste0(wd, "code/functions.R")) 
# source(paste0(wd, "code/cleanup_by_species.R"))
# source(paste0(wd, "code/species_sdms.R"))

# Additional functions ---------------------------------------------------------


#' Species distribution model function
#'
#' Function to create a mesh, fit the sdmTMB model, and get the index.
#' Exports fit.rds and index.rds files to the designated species folder.
#' Used for arrowtooth flounder, bocaccio, dover sole, lingcod north, lingcod
#' south, longnose skate, pacific ocean perch (pop), pacific spiny dogfish, rex
#' sole, yellowtail.
#'
#' @param x speciesname_df[[i]] which is a data frame from a list of data frames
#' created from the cleanup_by_species() function and any further post-processing
#' of depth filters (see the smaller_function.R file for those).
#' @param y speciesname_files[[i]] which is an item in a list created from
#' names(speciesname_df)
#' @import sdmTMB
#'
species_sdm_fn <- function(x, y, z, dir_spp) {
  # make mesh
  mesh <- sdmTMB::make_mesh(x, xy_cols = c("Longitude_dd", "Latitude_dd"), n_knots = 500)
  
  # fit model
  fit <- sdmTMB::sdmTMB(
    total_catch_wt_kg ~ 0 + factor(Year) + Pass,
    data = x,
    mesh = mesh,
    family = delta_gamma(),
    time = "Year",
    anisotropy = TRUE,
    spatiotemporal = as.list(c("iid", "iid"))
  )
  
  # get the index
  predictions <- predict(fit, newdata = z, return_tmb_object = TRUE) # 
  index <- sdmTMB::get_index(predictions, area = z$area_km2, bias_correct = TRUE)
  
  # save file
  saveRDS(fit, paste0(dir_spp, "fit_", y, ".rds"))
  saveRDS(index, paste0(dir_spp, "index_", y, ".rds"))
  
  return(list("fit" = fit, "predictions" = predictions, "index" = index))
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
                               test_species, 
                               seq_from = 0.1, 
                               seq_to = 1.0, 
                               seq_by = 0.1, 
                               tot_dataframes = 91, 
                               replicate_num = 10) {
  
  df <- catch %>% 
    dplyr::filter(
      Common_name == test_species$common_name)
  
  if (!is.na(test_species$filter_lat_lt) | is.null(test_species$filter_lat_lt)) {
    df <- df %>% dplyr::filter(Latitude_dd < test_species$filter_lat_lt)
  }
  if (!is.na(test_species$filter_lat_gt) | is.null(test_species$filter_lat_gt)) {
    df <- df %>% dplyr::filter(Latitude_dd > test_species$filter_lat_gt)
  }
  if (!is.na(test_species$filter_depth) | is.null(test_species$filter_depth)) {
    df <- df %>% dplyr::filter(Depth_m < test_species$filter_depth)
  }
  
  catch_split <- split(df, df$Year)
  
  tows <- lapply(catch_split, tow_fn)
  
  # Assign random 1s and 0s based on the specified proportions to a list of dataframes
  props <- as.data.frame(seq(from = seq_from, to = seq_to, by = seq_by))
  names(props) <- "Trawl_id"
  
  # match the structure of the catch data
  props <- rep(props, length(tows))
  
  tows_assigned <- map2(tows, props, include_or_exclude)
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


resample_tests <- function (spp_dfs = spp_dfs, test_species, grid_yrs, dir_out) {
  # set directories for outputs
  dir_spp <- paste0(dir_out, paste0(test_species$srvy, "_", test_species$file_name, "/"))
  dir.create(dir_spp, showWarnings = FALSE)
  
  all_fit_df <- all_fit_pars <- all_fit_check <- all_index <- list()
  
  spp_dfs <- spp_dfs[names(spp_dfs)[(length(names(spp_dfs))-1):length(names(spp_dfs))]] # reduce DFs for testing
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
    fit <- species_sdm_fn(x = spp_df, y = spp_files[[i]], z = grid_yrs, dir_spp = dir_spp)
    # Ensure extracted objects are dataframes, Store results in lists
    all_fit_df[[spp_files[[i]]]] <- cbind(test = spp_files[[i]], as.data.frame(fit_df_fn(fit)))
    fwrite(rbindlist(all_fit_df, fill = TRUE), file = paste0(dir_spp, "fit_df.csv"))
    all_fit_pars[[spp_files[[i]]]] <- cbind(test = spp_files[[i]], as.data.frame(fit_pars_fn(fit)))
    fwrite(rbindlist(all_fit_pars, fill = TRUE), file = paste0(dir_spp, "pars_df.csv"))
    all_fit_check[[spp_files[[i]]]] <- cbind(test = spp_files[[i]], as.data.frame(fit_check_fn(fit)))
    fwrite(rbindlist(all_fit_check, fill = TRUE), file = paste0(dir_spp, "fit_check_df.csv"))
    all_index[[spp_files[[i]]]] <- cbind(test = spp_files[[i]], as.data.frame(fit_check_fn(fit$index)))
    fwrite(rbindlist(all_index, fill = TRUE), file = paste0(dir_spp, "index_df.csv"))
    # Explicitly remove objects after processing
    rm(fit, spp_df, grid_yrs)
    gc()
    NULL
  }, .progress = TRUE, .options = furrr_options(seed = TRUE))
  
  print("...Parallel SDM processing complete")
  
  # ##### process index files
  # spp_indices <- pull_files(spp, "index")
  # spp_indices_df <- bind_index_fn(spp_indices)
  # write.csv(spp_indices_df, "spp_indices_df.csv", row.names = F)
  
  # Remove the rest of the files
  rm("spp_files", "spp_indices", "spp_indices_df")
  
  # #remove from memory
  # files_to_keep <- c("spp_fit_check_df.csv", "spp_fit_df.csv", "spp_pars_df.csv", "spp_indices_df.csv")
  # all_files <- list.files(path = ".", full.names = TRUE)  # Get all files
  # files_to_remove <- setdiff(all_files, file.path(".", files_to_keep))  # Exclude files to keep
  # file.remove(files_to_remove)  # Delete the files
}

#' Tow Function
#'
#' Create a vector of tows for including or excluding.
#'
#' @param x catch_split data frame
#'
tow_fn <- function(x) {
  tows <- as.data.frame(x$Trawl_id)
  tows <- unique(tows)
  tows <- as.data.frame(tows[!is.na(tows)])
  names(tows) <- "Trawl_id"
  return(tows)
}

#' Join Data Frames
#'
#' Function to join a list of data frames to a main data frame using a shared
#' column.
#'
#' @param list_of_d.fs The list of data frames that you would like to join to
#' the main data frame
#' @param main_df The main data frame that you want to join the list of data
#' frames
#' @param shared_column The column that all the data frames share which will be
#' used to join the data frames together.
#'
join_dfs <- function(list_of_dfs, main_df, shared_column) {
  merged_dfs <- lapply(list_of_dfs, function(df) {
    merged_df <- merge(df, main_df, by = shared_column)
    return(merged_df)
  })
  return(merged_dfs)
}

#' Create data frame of the species distribution model fit.
#'
#' Function to take the species distribution model fit and write it to a csv.
#'
#' @param fit Species distribution model fit from whatever smd_fn was used
#' (see species_sdms.R file for specifics on those sdm files).
#' @return main model fit data frame
#'
fit_df_fn <- function(fit) {
  fit_df <- tidy(fit, conf.int = T)
  return(fit_df)
  
  filename <- paste0(name(fit), "_fit_df.csv")
  write.csv(fit_df, file = filename, row.names = F)
}

#' Create data frame of the species distribution model parameter estimates.
#'
#' Function to get the species distribution model parameter estimates and write
#' it to a csv.
#'
#' @param fit Species distribution model fit from whatever smd_fn was used
#' (see species_sdms.R file for specifics on those sdm files).
#' @return parameter estimates data frame
#'
fit_pars_fn <- function(fit) {
  fit_pars <- tidy(fit, effects = "ran_pars", conf.int = T)
  return(fit_pars)
  
  filename <- paste0(name(fit), "_fit_pars.csv")
  write.csv(index, file = filename, row.names = F)
}

#' Create data frame of the species distribution model diagnostics.
#'
#' Function to get the species distribution model diagnostics and write
#' it to a csv.
#'
#' @param fit Species distribution model fit from whatever smd_fn was used
#' (see species_sdms.R file for specifics on those sdm files).
#' @return diagnostics data frame
#'
fit_check_fn <- function(fit) {
  fit_check <- sanity(fit)
  return(fit_check)
  
  filename <- paste0(name(fit), "_fit_check.csv")
  write.csv(index, file = filename, row.names = F)
}

#' Bind function for species distribution model fits and parameters
#'
#' Function to rbind and bring effort and replicates in as columns from
#' rownames of species distribution model fits and parameters.
#'
#' @param x Species distribution model fits or parameters data frame after
#' being read in by fit_df_fn or fit_pars_fn respectively.
#' @return New data frame of fits or parameters with the rownames as columns
#'
bind_fn <- function(x) {
  y <- do.call(rbind, x)
  y$effort <- gsub("fit_", "", row.names(y))
  y$replicate <- gsub(".*_", "", y$effort)
  y$effort <- gsub("_.*", "", y$effort)
  y$replicate <- gsub("\\..*", "", y$replicate)
  y$effort <- as.numeric(y$effort)
  y$replicate <- as.numeric(y$replicate)
  return(y)
}

#' Bind function for species distribution model diagnostics
#'
#' Function to rbind and bring effort and replicates in as columns from
#' rownames of species distribution model diagnostics.
#'
#' @param x Species distribution model diagnostics data frame after being read in
#' by fit_df_fn.
#' @return New data frame of diagnostics with the rownames as columns
#'
# special bind function for fit check dfs
bind_fit_check <- function(x) {
  y <- do.call(rbind, x)
  y <- as.data.frame(y)
  y$effort <- gsub("fit_", "", row.names(y))
  y$replicate <- gsub(".*_", "", y$effort)
  y$effort <- gsub("_.*", "", y$effort)
  y$replicate <- gsub("\\..*", "", y$replicate)
  y$effort <- as.numeric(y$effort)
  y$replicate <- as.numeric(y$replicate)
  y <- apply(y, 2, as.character)
  return(y)
}

#' Bind function for species distribution model index
#'
#' Function to rbind and bring effort and replicates in as columns from
#' rownames of species distribution model index.
#'
#' @param x Species distribution model index data frame after being read in.
#' @return New data frame of index with the rownames as columns
#'
bind_index_fn <- function(x) {
  y <- do.call(rbind, x)
  y$effort <- gsub(".rds.*", "", row.names(y))
  y$effort <- gsub("index_", "", y$effort)
  y$replicate <- gsub(".*_", "", y$effort)
  y$effort <- gsub("_.*", "", y$effort)
  y$effort <- as.numeric(y$effort)
  y$replicate <- as.numeric(y$replicate)
  return(y)
}

#' Pull files with specified character strings
#'
#' Function to read (pull) files from the set species directory containing
#' specified character strings.
#'
#' @param directory Species directory to look for files in.
#' @param string Character string to search files for.
#' @return Read in list of all files containing the character string searched for.
#'
pull_files <- function(directory, string) {
  # List all RDS files in the directory
  files <- list.files(directory, pattern = "\\.rds$", full.names = TRUE)
  
  # Filter files that contain the search string
  filtered_files <- files[grepl(string, files)]
  
  # Read in files and avoid unnecessary memory copies
  data_list <- vector("list", length(filtered_files))
  names(data_list) <- basename(filtered_files)
  
  for (i in seq_along(filtered_files)) {
    data_list[[i]] <- readRDS(filtered_files[i])
  }
  
  gc()  # Force garbage collection
  return(data_list)
}


# Run scenarios ----------------------------------------------------------------

## California Current ----------------------------------------------------------

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
  names(result_list) <- paste0(rep(proportions, each = replicate_num), "_", rep(1:replicate_num, times = length(proportions)))
  
  # Return the list of dataframes
  return(result_list)
}


### Define study species -------------------------------------------------------

test_species <- data.frame(srvy = "CA", 
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
      species_code = common_name)

### Load survey data -----------------------------------------------------------

catch_ca <- read.csv(paste0(wd,"data/nwfsc_bt_fmp_spp_updated.csv")) #pulled data again to get 2024
catch_ca <- catch_ca %>% 
  dplyr::select(Trawl_id, Common_name, Longitude_dd, Latitude_dd, Year, Pass, total_catch_wt_kg) %>% 
  dplyr::mutate(srvy = "CA")

### Load grid data -------------------------------------------------------------

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

### Variables ------------------------------------------------------------------

seq_from = 0.1
seq_to = 1
seq_by = 0.1
tot_dataframes = 91
grid_yrs <- grid_yrs_ca
replicate_num <- 10
catch <- catch_ca

### Run ------------------------------------------------------------------------

for (ii in 1:nrow(test_species)){
  print(paste0(test_species$srvy[ii], " ", test_species$common_name[ii]))
  spp_dfs <- cleanup_by_species(
    catch = catch, 
    test_species = test_species[ii,], 
    seq_from = seq_from, 
    seq_to = seq_to, 
    seq_by = seq_by, 
    tot_dataframes = tot_dataframes, 
    replicate_num = replicate_num)
  
  try({
    resample_tests(
      spp_dfs = spp_dfs, 
      test_species = test_species[ii,], 
      grid_yrs = grid_yrs, 
      dir_out = dir_out) 
  }, silent = FALSE)      # end of try function
}

## Alaska ----------------------------------------------------------------------

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
include_or_exclude <- function(df, proportions, replicate_num = 3) {
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
  names(result_list) <- paste0(rep(proportions, each = replicate_num), "_", rep(1:replicate_num, times = length(proportions)))
  
  # Return the list of dataframes
  return(result_list)
}


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
  filter_depth = NA) %>% 
  dplyr::mutate( 
    file_name = gsub(pattern = " ", replacement = "_", x = (tolower(common_name)))
  )

### Load survey data -----------------------------------------------------------

# source(paste0(wd, "code/data_dl_ak.r"))

load(file = paste0(wd, "/data/noaa_afsc_cpue.rda"))
catch_ak <- noaa_afsc_cpue

### Load grid data -------------------------------------------------------------

load(paste0(wd, "data/noaa_afsc_ebs_pred_grid_depth.rdata"))
noaa_afsc_ebs_pred_grid_depth <- noaa_afsc_ebs_pred_grid_depth %>% 
  dplyr::mutate(srvy = "EBS")
#make gridyrs
grid_yrs_ebs <- replicate_df(noaa_afsc_ebs_pred_grid_depth, "Year", unique(catch_ak$Year))

### Variables ------------------------------------------------------------------

seq_from = 0.2
seq_to = 1.0
seq_by = 0.2 
tot_dataframes = 13
grid_yrs <- grid_yrs_ebs
replicate_num <- 3
catch <- catch_ak

### Run ------------------------------------------------------------------------

for (ii in 1:nrow(test_species)){
  print(paste0(test_species$srvy[ii], " ", test_species$common_name[ii]))
  spp_dfs <- cleanup_by_species(
    catch = catch, 
    test_species = test_species[ii,], 
    seq_from = seq_from, 
    seq_to = seq_to, 
    seq_by = seq_by, 
    tot_dataframes = tot_dataframes, 
    replicate_num = replicate_num)
  
  try({
    resample_tests(
      spp_dfs = spp_dfs, 
      test_species = test_species[ii,], 
      grid_yrs = grid_yrs, 
      dir_out = dir_out) 
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