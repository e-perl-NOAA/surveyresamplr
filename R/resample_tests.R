

#' Resample Tests and Run SDM Processing
#'
#' This function resamples species data frames, runs species distribution models (SDMs) in parallel, and saves the results.
#'
#' @param spp_dfs A list of species data frames.
#' @param spp_info A data frame containing information about the test species.
#' @param grid_yrs A data frame or list containing grid years information.
#' @param dir_out A character string specifying the directory for output files.
#' 
#' #' @details
#' This function performs the following steps:
#' \itemize{
#'   \item Sets up directories for output files.
#'   \item Reduces the list of data frames to the last two entries for testing purposes.
#'   \item Saves each data frame in Parquet format.
#'   \item Sets up parallel processing using the \code{furrr} package.
#'   \item Runs species distribution models (SDMs) in parallel.
#'   \item Saves the results of the SDM processing into CSV files.
#' }
#' 
resample_tests <- function (spp_dfs, spp_info, grid_yrs, dir_out, test = FALSE) {
  # set directories for outputs
  dir_spp <- paste0(dir_out, paste0(spp_info$srvy, "_", spp_info$file_name, "/"))
  dir.create(dir_spp, showWarnings = FALSE)
  
  if (test) {
    spp_dfs <- spp_dfs[names(spp_dfs)[(length(names(spp_dfs))-1):length(names(spp_dfs))]] # reduce DFs for testing
  }
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
  
  message("...Starting parallel SDM processing")
  
  # Run SDMs in parallel
  future_map(seq_along(spp_files), function(i) {
    message(paste0("\n...", spp_files[[i]], "\n"))
    gc()  # Free memory
    # Load only the required dataframe
    spp_df <- read_parquet(paste0(dir_spp, paste0("df_", i, ".parquet")))
    # Run species SDM function
    fit0 <- species_sdm_wrapper(
      x = spp_df, 
      y = spp_files[[i]], 
      z = grid_yrs, 
      dir_spp = dir_spp, 
      spp_info = spp_info)
    # fit <- readRDS(file = paste0(dir_spp, "fit_", spp_files[[i]], ".rds")) # for testing
    # index <- readRDS(file = paste0(dir_spp, "index_", spp_files[[i]], ".rds")) # for testing
    # fit0 <- list("fit" = fit, "index" = index)
    # Ensure extracted objects are dataframes, Store results in lists
    # fit 
    if (!file.exists(paste0(dir_spp, "fit_df.csv"))) {
      fit_df <- c()
    } else {
      fit_df <- read.csv(file = paste0(dir_spp, "fit_df.csv")) %>%  
        dplyr::mutate(across(everything(), as.character))
    }
    fit_df <- fit_df %>%
      dplyr::bind_rows(
        dplyr::bind_cols(
          spp_info %>% 
            dplyr::mutate(effort = as.character(spp_files[[i]])), 
          data.frame(
            data.frame(fit_df_fn(fit0$fit))) ) %>%
          dplyr::mutate(across(everything(), as.character)) 
      )
    fwrite(fit_df, file = paste0(dir_spp, "fit_df.csv"))
    # fit pars
    if (!file.exists(paste0(dir_spp, "fit_pars.csv"))) {
      fit_pars <- c()
    } else {
      fit_pars <- read.csv(file = paste0(dir_spp, "fit_pars.csv")) %>%  
        dplyr::mutate(across(everything(), as.character))
    }
    fit_pars <- fit_pars %>%
      dplyr::bind_rows(
        dplyr::bind_cols(
          spp_info %>% 
            dplyr::mutate(effort = as.character(spp_files[[i]])), 
          data.frame(
            data.frame(fit_pars_fn(fit0$fit))) ) %>%
          dplyr::mutate(across(everything(), as.character)) 
      )
    fwrite(fit_pars, file = paste0(dir_spp, "fit_pars.csv"))
    # fit check
    if (!file.exists(paste0(dir_spp, "fit_check.csv"))) {
      fit_check <- c()
    } else {
      fit_check <- read.csv(file = paste0(dir_spp, "fit_check.csv")) %>%  
        dplyr::mutate(across(everything(), as.character))
    }
    fit_check <- fit_check %>%
      dplyr::bind_rows(
        dplyr::bind_cols(
          spp_info %>% 
            dplyr::mutate(effort = as.character(spp_files[[i]])), 
          data.frame(
            data.frame(fit_check_fn(fit0$fit))) ) %>%
          dplyr::mutate(across(everything(), as.character)) 
      )
    fwrite(fit_check, file = paste0(dir_spp, "fit_check.csv"))
    # index
    if (!file.exists(paste0(dir_spp, "index.csv"))) {
      index <- c()
    } else {
      index <- read.csv(file = paste0(dir_spp, "index.csv")) %>%  
        dplyr::mutate(across(everything(), as.character))
    }
    index <- index %>%
      dplyr::bind_rows(
        dplyr::bind_cols(
          spp_info %>% 
            dplyr::mutate(effort = as.character(spp_files[[i]])), 
          data.frame(
            data.frame(fit0$index)) ) %>%
          dplyr::mutate(across(everything(), as.character)) 
      )
    fwrite(index, file = paste0(dir_spp, "index.csv"))
    # Explicitly remove objects after processing
    rm("fit0", "spp_df")
    gc()
    # NULL
  }, .progress = TRUE, .options = furrr_options(seed = TRUE))
  
  message("...Parallel SDM processing complete")
}

