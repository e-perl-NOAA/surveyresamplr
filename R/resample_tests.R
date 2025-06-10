resample_tests <- function(spp_dfs, spp_info, grid_yrs, dir_out, test = FALSE, model_type = "wrapper_sdmtmb") {
  # If test mode, only process last 2 dataframes
  if (test) {
    spp_dfs <- spp_dfs[names(spp_dfs)[(length(names(spp_dfs)) - 1):length(names(spp_dfs))]]
  }
  
  # Create output directory 
  dir_spp <- file.path(dir_out, paste0(spp_info$srvy, "_", spp_info$file_name))
  dir.create(dir_spp, recursive = TRUE, showWarnings = FALSE)
  
  # Run model for each dataset
  spp_files <- as.list(names(spp_dfs))
  for (i in seq_along(spp_dfs)) {
    # Get wrapper function
    wrapper_fn <- get(model_type)
    
    # Write data to parquet file
    arrow::write_parquet(spp_dfs[[i]], file.path(dir_spp, paste0("df_", i, ".parquet")))
    
    # Run model
    wrapper_fn(x = spp_dfs[[i]], y = spp_files[[i]], z = grid_yrs,
              dir_spp = dir_spp, spp_info = spp_info)
  }
}