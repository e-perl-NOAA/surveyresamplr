clean_and_resample <- function(spp_info, catch, seq_from, seq_to, seq_by, tot_dataframes,
                               replicate_num, grid_yrs, dir_out, test = FALSE) {
  # Clean, filter, and resample data for modeling
  spp_dfs <- cleanup_by_species(
    catch = catch, spp_info = spp_info,
    seq_from = seq_from, seq_to = seq_to, seq_by = seq_by,
    tot_dataframes = tot_dataframes, replicate_num = replicate_num
  )

  # Run the resampling tests
  if (nrow(spp_dfs[[1]]) > 0) {
    resample_tests(
      spp_dfs = spp_dfs, spp_info = spp_info, grid_yrs = grid_yrs,
      dir_out = dir_out, test = test, model_type = "wrapper_sdmtmb"
    )
  } else {
    message("No data available for this species")
  }
}
