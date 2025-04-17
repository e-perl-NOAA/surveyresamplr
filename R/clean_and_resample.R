

#' Clean and Resample Species Data
#'
#' This function cleans up the catch data for a specific species and then performs resampling tests.
#'
#' @param spp_info A data frame row containing information about the species.
#' @param catch A data frame containing the catch data.
#' @param seq_from A numeric value specifying the start of the sequence for data frames.
#' @param seq_to A numeric value specifying the end of the sequence for data frames.
#' @param seq_by A numeric value specifying the step size of the sequence for data frames.
#' @param tot_dataframes An integer specifying the total number of data frames to generate.
#' @param replicate_num An integer specifying the number of replicates.
#' @param grid_yrs A data frame or list containing grid years information.
#' @param dir_out A character string specifying the directory for output files.
#' @param test Logical. TRUE/FALSE. If TRUE, will only run first two resampling tests. 
#'
#' @details
#' This function performs the following steps:
#' \itemize{
#'   \item Cleans up the catch data for the specified species using `cleanup_by_species`.
#'   \item Performs resampling tests on the cleaned data using `resample_tests`.
#' }
#' 
clean_and_resample <- function(spp_info, catch, seq_from, seq_to, seq_by, tot_dataframes, replicate_num, grid_yrs, dir_out, test = FALSE) {
  
  message(paste0(spp_info$srvy, " ", spp_info$common_name))
  
  # check input variables
  ## do all of the model function variables exist in grid_yrs and the catch data?
  aa <- spp_info$model_fn
  aa <- gsub(x = aa, pattern = "factor(", replacement = "", fixed = TRUE)
  aa <- gsub(x = aa, pattern = ")", replacement = "", fixed = TRUE)  
  aa <- strsplit(x = aa, split = " ")[[1]]
  aa <- aa[which(!(aa %in% c("+", "0", "~", "total_catch_wt_kg")))]
  
  if (sum(aa %in% names(grid_yrs)) != length(aa)){
    stop(paste0("ERROR: Not all variables called in funciton are available in the grid_yrs object: ", aa[!(aa %in% names(grid_yrs))]))
  }
  
  if (sum(aa %in% names(catch)) != length(aa)){
    stop(paste0("ERROR: Not all variables called in funciton are available in the catch data object: ", aa[!(aa %in% names(catch))]))
  }
  
  spp_dfs <- cleanup_by_species(
    catch = catch, 
    spp_info = spp_info, 
    seq_from = seq_from, 
    seq_to = seq_to, 
    seq_by = seq_by, 
    tot_dataframes = tot_dataframes, 
    replicate_num = replicate_num
  )
  
  try({
    resample_tests(
      spp_dfs = spp_dfs, 
      spp_info = spp_info, 
      grid_yrs = grid_yrs, 
      dir_out = dir_out, 
      test = test
    ) 
  }, silent = FALSE)
}

