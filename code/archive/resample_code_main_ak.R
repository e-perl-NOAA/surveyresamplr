# NOTES ------------------------------------------------------------------------
# Resample_survey_data: Multiple species, multiple years
# Em Markowitz March 2025
# Alaska Fisheries
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
  "doParallel",
  
  # sampling
  "sampling",
  
  # modeling
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

# Set directories --------------------------------------------------------------
wd<- "Z:/Projects/Resample-survey-data/"
# wd <- paste0(here::here(), "/")
wd_results <- paste0(wd, "/Results/alaska/")
dir.create(wd_results, showWarnings = FALSE)
wd_results_v <- paste0(wd_results, "/", Sys.Date() ,"/")
dir.create(wd_results_v, showWarnings = FALSE)

# Load source files ------------------------------------------------------------

# source(paste0(wd, "code/data_dl_ak.R"))
load(file = paste0(wd, "/data/noaa_afsc_public_foss.rda"))

source(here::here("smaller_functions.R")) #need to edit to specify the code directory if running locally
source(here::here("cleanup_by_species.R"))
source(here::here("species_sdms.R"))

# Run scenarios ----------------------------------------------------------------
for (i in 1:nrow(test_species)) {
  dir_spp <- paste0(wd_results_v, test_species$file_name[i])
  dir.create(dir_spp, showWarnings = FALSE)
  setwd(dir_spp)
  
  spp_dfs <- cleanup_by_species(df = data_survey, species = test_species$common_name[i], 
                                       seq_from = 0.2, seq_to = 1.0, seq_by = 0.2, 
                                       tot_dataframes = 13, include_or_exclude = include_or_exclude_ak)
spp_dfs <- lapply(spp_dfs, lat_filter_34)
#spp_dfs <- spp_dfs[90:91] # reduce DFs for testing

# make the names file
spp_files <- as.list(names(spp_dfs))

# setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1] - 1) # to not overload your computer
registerDoParallel(cl)

# with predefined function
print("Starting parallel SDM processing")
spp_sdms <- foreach(i = seq_along(spp_dfs), .combine = "list", 
                    .packages = c("foreach", "doParallel", "sdmTMB"), 
                    .errorhandling = "remove") %dopar% {
  print(paste("Processing SDM:", i))
  result <- species_sdm_fn(spp_dfs[[i]], spp_files[[i]], grid_yrs)
  print(paste("Result for SDM", i, ":", result))
  result
}
stopCluster(cl)
print("Parallel SDM processing complete")

##### read in fit files
spp_sdms <- pull_files(arrowtooth, "fit")

# Extract outputs and save to csvs
spp_fits <- lapply(spp_sdms, fit_df_fn)
spp_fit_df <- bind_fn(spp_fits)
write.csv(spp_fit_df, "spp_fit_df.csv", row.names = F)

spp_pars <- lapply(spp_sdms, fit_pars_fn)
spp_pars_df <- bind_fn(spp_pars)
write.csv(spp_pars_df, "spp_pars_df.csv", row.names = F)

spp_fit_check <- lapply(spp_sdms, fit_check_fn)
spp_fit_check_df <- bind_fit_check(spp_fit_check)
write.csv(spp_fit_check_df, "spp_fit_check_df.csv", row.names = F)

# Remove some files to help with memory
rm(
  "spp_fits", "spp_fit_df", "spp_pars",
  "spp_pars_df", "spp_fit_check", "spp_fit_check_df"
)

##### read in index files
spp_indices <- pull_files(arrowtooth, "index")
spp_indices_df <- bind_index_fn(spp_indices)
write.csv(spp_indices_df, "spp_indices_df.csv", row.names = F)

# Remove the rest of the files
rm("spp_dfs", "spp_files", "spp_sdms", "spp_indices", "spp_indices_df")

}