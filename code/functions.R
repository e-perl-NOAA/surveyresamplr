##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Resample_survey_data: Multiple species, multiple years
## Authors:       Derek Bolser, Office of Science and Technology (derek.bolser@noaa.gov)
##                Em Markowitz, Alaska Fisheries Science Center (emily.markowitz@noaa.gov)
##                Elizabeth Perl, ECS Federal contracted to Office of Science and Technology (elizabeth.gugliotti@noaa.gov)
## Description:   Resample_survey_data: Multiple species, multiple years.
## Date:          March 2025
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Package install
#' 
#' @param p name of package
#' On google workstations you can use the library install bash script Elizabeth created. 
#' Copy that into you working
#' directory, run 'chmod u+x ubuntu_libraries.sh' and then run './ubuntu_libraries.sh'
#' @example 
#' pkgs <- c("dplyr, "nwfscSurvey", "sdmTMB)
#' lapply(pkgs, pkg_install)
#' 
pkg_install <- function(p){
  if(grepl("/home/user/", getwd())){
    system("chmod a+x ubuntu_libraries.sh")
    system("./ubuntu_libraries.sh")
  }
  if(!require(p, character.only = TRUE)) {
    if (p == 'coldpool') {
      devtools::install_github("afsc-gap-products/coldpool")
    } else if (p == "akgfmapas") {
      devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
    } else if (p == 'nwfscSurvey') {
      remotes::install_github("pfmc-assessments/nwfscSurvey")
    } else {
      install.packages(p)
    }
    require(p, character.only = TRUE)}
}

#' Include or Exclude
#'
#' Specify how to downsample. For simple random sampling, a proportion of
#' stations should do.
#'
#' @param df tows data frame
#' @param proportions proportions developed using: props <- as.data.frame(seq
#' (0.1,1.0, by = 0.1)) replicated by the length of the tows dataframe. The name
#' of the props is "Trawl_id".
#' @param replicate_num going to be 10 for NWFSC and 3 for AK
#'
include_or_exclude <- function(df, proportions, replicate_num) {
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


#' Cleanup species function
#'
#' Filter catch by species, create a vector of tows and give tows a random
#' assignment to be resampled,
#'
#' @param catch full catch df
#' @param species_row A data frame containing information about the test species.
#' @param seq_from
#' @param seq_to
#' @param seq_by
#' @param tot_dataframes the number of data frames you want to output. effort x replicates - (replicates - 1). 5x3-2
#' @param replicate_num
#' @return List of resampled catch data frames
#'
cleanup_by_species <- function(catch, 
                               species_row, 
                               seq_from = 0.1, 
                               seq_to = 1.0, 
                               seq_by = 0.1, 
                               tot_dataframes = 91, 
                               replicate_num = 10) {
  
  df <- catch %>% 
    dplyr::filter(
      Common_name == species_row$common_name)
  
  if (!is.na(species_row$filter_lat_lt) | is.null(species_row$filter_lat_lt)) {
    df <- df %>% dplyr::filter(Latitude_dd < species_row$filter_lat_lt)
  }
  if (!is.na(species_row$filter_lat_gt) | is.null(species_row$filter_lat_gt)) {
    df <- df %>% dplyr::filter(Latitude_dd > species_row$filter_lat_gt)
  }
  if (!is.na(species_row$filter_depth) | is.null(species_row$filter_depth)) {
    df <- df %>% dplyr::filter(Depth_m < species_row$filter_depth)
  }
  
  catch_split <- split(df, df$Year)
  
  tows <- lapply(catch_split, tow_fn)
  
  # Assign random 1s and 0s based on the specified proportions to a list of dataframes
  props <- as.data.frame(seq(from = seq_from, to = seq_to, by = seq_by))
  names(props) <- "Trawl_id"
  
  # match the structure of the catch data
  props <- rep(props, length(tows))
  
  tows_assigned <- map2(tows, props, include_or_exclude, replicate_num = replicate_num)
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
    dplyr::bind_rows(.id = "source")
  
  species_all_yrs <- split(species_all_yrs, species_all_yrs$source)
  names(species_all_yrs) <- gsub(x = names(species_all_yrs), pattern = ".", replacement = "", fixed = TRUE)
  
  rm("df", "catch_split", "tows", "props", "tows_assigned", "alldata_resampled")
  
  return(species_all_yrs)
}


#' Resample Tests and Run SDM Processing
#'
#' This function resamples species data frames, runs species distribution models (SDMs) in parallel, and saves the results.
#'
#' @param spp_dfs A list of species data frames.
#' @param species_row A data frame containing information about the test species.
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
resample_tests <- function (spp_dfs, species_row, grid_yrs, dir_out) {
  # set directories for outputs
  dir_spp <- paste0(dir_out, paste0(species_row$srvy, "_", species_row$file_name, "/"))
  dir.create(dir_spp, showWarnings = FALSE)
  
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
  
  message("...Starting parallel SDM processing")
  
  assign(value = get(species_row$model_fn), x = "model_function")
  
  # Run SDMs in parallel
  future_map(seq_along(spp_files), function(i) {
    message(paste0("\n...", spp_files[[i]], "\n"))
    gc()  # Free memory
    # Load only the required dataframe
    spp_df <- read_parquet(paste0(dir_spp, paste0("df_", i, ".parquet")))
    # Run species SDM function
    fit0 <- model_function(x = spp_df, y = spp_files[[i]], z = grid_yrs, dir_spp = dir_spp)
    # fit <- readRDS(file = paste0(dir_spp, "fit_", spp_files[[i]], ".rds")) # for testing
    # Ensure extracted objects are dataframes, Store results in lists
    # fit 
    if (!file.exists(paste0(dir_spp, "fit_df.csv"))) {fit_df <- c()} else {fit_df <- read.csv(file = paste0(dir_spp, "fit_df.csv"))}
    fit_df <- fit_df %>%  
      dplyr::bind_rows(
        dplyr::bind_cols(
          species_row, 
          data.frame(
            effort = spp_files[[i]],
            data.frame(fit_df_fn(fit0$fit)))))
    fwrite(fit_df, file = paste0(dir_spp, "fit_df.csv"))
    # fit pars
    if (!file.exists(paste0(dir_spp, "fit_pars.csv"))) {fit_pars <- c()} else {fit_pars <- read.csv(file = paste0(dir_spp, "fit_pars.csv"))}
    fit_pars <- fit_pars %>%  
      dplyr::bind_rows(
        dplyr::bind_cols(
          species_row, 
          data.frame(
            effort = spp_files[[i]],
            data.frame(fit_pars_fn(fit0$fit)))))
    fwrite(fit_pars, file = paste0(dir_spp, "fit_pars.csv"))
    # fit check
    if (!file.exists(paste0(dir_spp, "fit_check.csv"))) {fit_check <- c()} else {fit_check <- read.csv(file = paste0(dir_spp, "fit_check.csv"))}
    fit_check <- fit_check %>%  
      dplyr::bind_rows(
        dplyr::bind_cols(
          species_row, 
          data.frame(
            effort = spp_files[[i]],
            data.frame(fit_check_fn(fit0$fit)))))
    fwrite(fit_check, file = paste0(dir_spp, "fit_check.csv"))
    # index
    if (!file.exists(paste0(dir_spp, "index.csv"))) {index <- c()} else {index <- read.csv(file = paste0(dir_spp, "index.csv"))}
    index <- index %>%  
      dplyr::bind_rows(
        dplyr::bind_cols(
          species_row, 
          data.frame(
            effort = spp_files[[i]],
            data.frame(fit0$index))))
    fwrite(index, file = paste0(dir_spp, "index.csv"))
    # Explicitly remove objects after processing
    rm("fit0", "spp_df")
    gc()
    # NULL
  }, .progress = TRUE, .options = furrr_options(seed = TRUE))
  
  message("...Parallel SDM processing complete")
}

#' Clean and Resample Species Data
#'
#' This function cleans up the catch data for a specific species and then performs resampling tests.
#'
#' @param species_row A data frame row containing information about the species.
#' @param catch A data frame containing the catch data.
#' @param seq_from A numeric value specifying the start of the sequence for data frames.
#' @param seq_to A numeric value specifying the end of the sequence for data frames.
#' @param seq_by A numeric value specifying the step size of the sequence for data frames.
#' @param tot_dataframes An integer specifying the total number of data frames to generate.
#' @param replicate_num An integer specifying the number of replicates.
#' @param grid_yrs A data frame or list containing grid years information.
#' @param dir_out A character string specifying the directory for output files.
#'
#' @details
#' This function performs the following steps:
#' \itemize{
#'   \item Cleans up the catch data for the specified species using `cleanup_by_species`.
#'   \item Performs resampling tests on the cleaned data using `resample_tests`.
#' }
#' 
clean_and_resample <- function(species_row, catch, seq_from, seq_to, seq_by, tot_dataframes, replicate_num, grid_yrs, dir_out) {
  message(paste0(species_row$srvy, " ", species_row$common_name))
  
  spp_dfs <- cleanup_by_species(
    catch = catch, 
    species_row = species_row, 
    seq_from = seq_from, 
    seq_to = seq_to, 
    seq_by = seq_by, 
    tot_dataframes = tot_dataframes, 
    replicate_num = replicate_num
  )
  
  try({
    resample_tests(
      spp_dfs = spp_dfs, 
      species_row = species_row, 
      grid_yrs = grid_yrs, 
      dir_out = dir_out
    ) 
  }, silent = FALSE)
}


#' Plot Results and Save Figures
#'
#' This function compiles results from species distribution models (SDMs), generates plots, and saves the figures.
#'
#' @param srvy A character string specifying the survey identifier.
#' @param dir_out A character string specifying the directory for output files.
#'
#' @details
#' This function performs the following steps:
#' \itemize{
#'   \item Creates a directory for saving images.
#'   \item Searches for relevant files in the output directory.
#'   \item Compiles data from the found files.
#'   \item Saves the compiled data into CSV files.
#'   \item Generates various plots such as boxplots and time series of biomass estimates.
#'   \item Saves the generated plots as PNG files.
#'   \item Saves the list of plots in an RData file.
#' }
#'
plot_results <- function(srvy, dir_out) {
  
  # create directory for images to be saved to
  dir_fig <- paste0(dir_out, paste0(srvy, "_0figures/"))
  dir.create(dir_fig, showWarnings = FALSE)
  
  # find files
  aaa <- list.files(path = dir_out, pattern = srvy, full.names = TRUE)
  aaa <- aaa[!grepl(pattern = "figures", x = aaa)]
  
  # compile files for each org
  fit_df <- fit_pars <- fit_check <- index <- data.frame()
  for (i in 1:length(aaa)) {
    if (file.exists(paste0(aaa[i], "/fit_df.csv"))) {
      fit_df <- fit_df %>% 
        dplyr::bind_rows(read.csv(paste0(aaa[i], "/fit_df.csv")))
    }    
    if (file.exists(paste0(aaa[i], "fit_pars.csv"))) {
      fit_pars <- fit_pars %>% 
        dplyr::bind_rows(read.csv(paste0(aaa[i], "/fit_pars.csv")))
    }
    if (file.exists(paste0(aaa[i], "/fit_check.csv"))) {
      fit_check <- fit_check %>% 
        dplyr::bind_rows(read.csv(paste0(aaa[i], "/fit_check.csv")))
    }
    if (file.exists(paste0(aaa[i], "/index.csv"))) {
      index <- index %>% 
        dplyr::bind_rows(read.csv(paste0(aaa[i], "/index.csv")))
    }
  }
  
  write.csv(x = fit_df, file = paste0(dir_fig, "/fit_df.csv"))
  write.csv(x = fit_pars, file = paste0(dir_fig, "/fit_pars.csv"))
  write.csv(x = fit_check, file = paste0(dir_fig, "/fit_check.csv"))
  write.csv(x = index, file = paste0(dir_fig, "/index.csv"))
  
  # plotting -------------------------------------------------------------------
  
  plot_list <- c()
  i <- 0
  theme_custom <- theme_bw() + 
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
  
  ## log biomass estimates boxplot ---------------------------------------------
  p1 <- ggplot2::ggplot(
    data = index, 
    mapping = aes(
      x = as.factor(effort), 
      y = log_est, 
      color = effort)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~ common_name) +
    ggplot2::labs(x = "Proprotion of effort",
         y = "Log biomass estimate")  + 
    ggplot2::scale_color_viridis_d(
      name = "Effort", 
      option = "D") +
    theme_custom
  
  i <- i + 1; plot_list[[i]] <- p1
  names(plot_list)[i] <- 'index_boxplot_log_biomass.png'
  
  # log(?) SE boxplot ----------------------------------------------------------
  p1 <- ggplot2::ggplot(
    data = index, 
    mapping = aes(
      x = as.factor(effort), 
      y = se, 
      color = effort)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~ common_name) +
    ggplot2::labs(x = "Proprotion of effort",
         y = "Standard error of log biomass estimate")  + 
    ggplot2::scale_color_viridis_d(
      name = "Effort", 
      option = "D") +
    theme_custom
  
  i <- i + 1; plot_list[[i]] <- p1
  names(plot_list)[i] <- 'index_boxplot_log_biomass_SE.png'
  
  # biomass estimates boxplot --------------------------------------------------
  p1 <- ggplot2::ggplot(
    data = index, 
    mapping = aes(
      x = as.factor(effort), 
      y = est, 
      color = effort)) +
    geom_boxplot() +
    ggplot2::facet_wrap(~ common_name) +
    ggplot2::labs(x = "Proprotion of effort",
         y = "Biomass estimate")  + 
    ggplot2::scale_color_viridis_d(
      name = "Effort", 
      option = "D") +
   theme_custom
  
  i <- i + 1; plot_list[[i]] <- p1
  names(plot_list)[i] <- 'index_boxplot_biomass.png'
  
  # biomass estimates over time ---------------------------------------------------
  p1 <- ggplot2::ggplot(
    data = index, 
               mapping = aes(
                 x = Year, 
                 y = est, 
                 color = effort)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ common_name) +
    ggplot2::labs(x = "Years",
         y = "Biomass estimate")  + 
    ggplot2::scale_color_viridis_d(
      name = "Effort", 
      option = "D") +
    theme_custom + 
    ggplot2::theme(legend.position = "right")
  
  i <- i + 1; plot_list[[i]] <- p1
  names(plot_list)[i] <- 'index_timeseries_biomass.png'
  
  for (ii in 1:length(plot_list)) {
    ggsave(filename = names(plot_list)[ii],
           plot = plot_list[[ii]], 
           path = dir_fig, 
           width = 8, 
           height = 8, 
           device = 'png', 
           dpi = 300)
  }
  
  save(plot_list, file = paste0(dir_fig, "figures.rdata"))
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



