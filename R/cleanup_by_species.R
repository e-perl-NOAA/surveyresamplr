cleanup_by_species <- function(catch, spp_info, seq_from, seq_to, seq_by, tot_dataframes, replicate_num) {
  # Filter data by species
  spp_catch <- catch[catch$common_name == spp_info$common_name, ]
  
  # Apply any latitude filters
  if (!is.na(spp_info$filter_lat_gt)) {
    spp_catch <- spp_catch[spp_catch$latitude_dd > spp_info$filter_lat_gt, ]
  }
  if (!is.na(spp_info$filter_lat_lt)) {
    spp_catch <- spp_catch[spp_catch$latitude_dd < spp_info$filter_lat_lt, ]  
  }
  
  # Apply depth filter if specified
  if (!is.na(spp_info$filter_depth)) {
    spp_catch <- spp_catch[spp_catch$depth_m < spp_info$filter_depth, ]
  }
  
  if (nrow(spp_catch) == 0) {
    return(list(data.frame()))
  }
  
  # Get unique tow IDs
  tows <- tow_fn(spp_catch)
  
  # Generate resampled datasets
  resamples <- include_or_exclude(tows, 
    proportions = seq(seq_from, seq_to, seq_by),
    replicate_num = replicate_num)
  
  # Join resampled tow IDs back to main data
  join_dfs(resamples, spp_catch, "trawlid")
}

# Helper function to get unique, non-NA tow IDs
tow_fn <- function(data) {
  data.frame(trawlid = unique(data$trawlid[!is.na(data$trawlid)]))
}

# Helper function to join resampled data back to original data
join_dfs <- function(list_dfs, main_df, join_col) {
  lapply(list_dfs, function(x) merge(x, main_df, by = join_col))
}